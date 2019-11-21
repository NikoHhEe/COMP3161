module MinHS.TyInfer where

  import qualified MinHS.Env as E
  import MinHS.Syntax
  import MinHS.Subst
  import MinHS.TCMonad
  
  import Data.Monoid (Monoid (..), (<>))
  import Data.Foldable (foldMap)
  import Data.List (nub, union, (\\))
  
  primOpType :: Op -> QType
  primOpType Gt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Ge   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Lt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Le   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Eq   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Ne   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Neg  = Ty $ Base Int `Arrow` Base Int
  primOpType Fst  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "a"
  primOpType Snd  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "b"
  primOpType _    = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Int)
  
  constType :: Id -> Maybe QType
  constType "True"  = Just $ Ty $ Base Bool
  constType "False" = Just $ Ty $ Base Bool
  constType "()"    = Just $ Ty $ Base Unit
  constType "Pair"  = Just
                    $ Forall "a"
                    $ Forall "b"
                    $ Ty
                    $ TypeVar "a" `Arrow` (TypeVar "b" `Arrow` (TypeVar "a" `Prod` TypeVar "b"))
  constType "Inl"   = Just
                    $ Forall "a"
                    $ Forall "b"
                    $ Ty
                    $ TypeVar "a" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
  constType "Inr"   = Just
                    $ Forall "a"
                    $ Forall "b"
                    $ Ty
                    $ TypeVar "b" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
  constType _       = Nothing
  
  type Gamma = E.Env QType
  
  initialGamma :: Gamma
  initialGamma = E.empty
  
  tv :: Type -> [Id]
  tv = tv'
   where
     tv' (TypeVar x) = [x]
     tv' (Prod  a b) = tv a `union` tv b
     tv' (Sum   a b) = tv a `union` tv b
     tv' (Arrow a b) = tv a `union` tv b
     tv' (Base c   ) = []
  
  tvQ :: QType -> [Id]
  tvQ (Forall x t) = filter (/= x) $ tvQ t
  tvQ (Ty t) = tv t
  
  tvGamma :: Gamma -> [Id]
  tvGamma = nub . foldMap tvQ
  
  infer :: Program -> Either TypeError Program
  infer program = do (p',term, s) <- runTC $ inferProgram initialGamma program
                     return p'
  
  unquantify :: QType -> TC Type
  {-
  Normally this implementation would be possible:
  unquantify (Ty t) = return t
  unquantify (Forall x t) = do x' <- fresh
                               unquantify (substQType (x =:x') t)
  However as our "fresh" names are not checked for collisions with names bound in the type
  we avoid capture entirely by first replacing each bound
  variable with a guaranteed non-colliding variable with a numeric name,
  and then substituting those numeric names for our normal fresh variables
  -}
  
  unquantify = unquantify' 0 emptySubst
  unquantify' :: Int -> Subst -> QType -> TC Type
  unquantify' i s (Ty t) = return $ substitute s t
  unquantify' i s (Forall x t) = do x' <- fresh
                                    unquantify' (i + 1)
                                                ((show i =: x') <> s)
                                                (substQType (x =:TypeVar (show i)) t)
  
  unify :: Type -> Type -> TC Subst
  unify (TypeVar tv1) (TypeVar tv2) = if tv1 == tv2 then
                                      return emptySubst
                                    else
                                      return (tv1 =: (TypeVar tv2))
  unify (Base b1) (Base b2) = if b1 == b2 then
                                return emptySubst
                              else
                                typeError (TypeMismatch (Base b1) (Base b2))
  unify (Prod p1 p2) (Prod p3 p4) = do
                          q1 <- unify p1 p3
                          q2 <- unify (substitute q1 p2) (substitute q1 p4)
                          return (q1 <> q2)
  unify (Sum s1 s2) (Sum s3 s4) = do
                          t1 <- unify s1 s3
                          t2 <- unify (substitute t1 s2) (substitute t1 s4)
                          return (t1 <> t2)
  unify (Arrow a1 a2) (Arrow a3 a4) = do
                          b1 <- unify a1 a3
                          b2 <- unify (substitute b1 a2) (substitute b1 a4)
                          return (b1 <>b2)
  unify (TypeVar tvar) n =  if (elem tvar (tv n)) then
                            typeError (OccursCheckFailed tvar n)
                          else
                            return (tvar =: n) 
  unify n (TypeVar tvar) =  if (elem tvar (tv n)) then
                              typeError (OccursCheckFailed tvar n)
                            else
                              return (tvar =: n)
  
  generalise :: Gamma -> Type -> QType
  generalise ga tp = convert ((tvQ (Ty tp)) \\ (tvGamma ga)) tp
                    
  convert :: [Id] -> Type -> QType
  convert [] tp = Ty tp
  convert (x : xs) tp = Forall x (convert xs tp)


  inferProgram :: Gamma -> Program -> TC (Program, Type, Subst)
  inferProgram ga [Bind n _ [] expr] = do 
    (p, tp, sub) <- inferExp ga expr 
    case generalise ga (substitute sub tp) of 
      Ty ty1 -> return ([Bind "main" (Just (Ty (substitute sub tp))) [] p], substitute sub tp, sub)
      ty2 -> return ([Bind "main" (Just ty2) [] p], substitute sub tp, sub)
    
  inferExp :: Gamma -> Exp -> TC (Exp, Type, Subst)
  inferExp ga (Num n) = return (Num n, Base Int, emptySubst)
  
  -- Variables
  inferExp ga (Var a) = do
    case E.lookup ga a of 
      Just t1 -> do 
              t2  <- unquantify t1
              return (Var a, t2, emptySubst)
      Nothing -> typeError (NoSuchVariable a) 
  
  -- Constructor types
  inferExp ga (Con tp) = do
    case constType tp of 
      Just t1 -> do 
        t2 <- unquantify t1
        return (Con tp, t2, emptySubst)
      Nothing -> typeError (NoSuchConstructor tp) 
  
  -- Primary operator
  inferExp ga (Prim op) = do 
    t <- unquantify (primOpType op)
    return (Prim op, t, emptySubst)
  
  -- App expression 
  inferExp ga (App exp1 exp2) = do 
    (exp1', t1, sub) <- inferExp ga exp1
    (exp2', t2, sub') <- inferExp (substGamma sub ga) exp2
    alpha <- fresh 
    un <- unify (substitute sub' t1) (Arrow t2 alpha)
    return (App (allTypes (substQType (un <> sub')) exp1') exp2', 
                 substitute un alpha, un <> sub' <> sub)
  
  -- If expression
  inferExp ga (If c et ee) = do 
    (c', tp, sub)  <- inferExp ga c
    un <- unify tp (Base Bool)
    case substitute (un <> sub) tp of 
      Base Bool -> do
        (et', t1, s1)   <- inferExp (substGamma (un <> sub) ga) et
        (ee', t2, s2)   <- inferExp (substGamma (s1 <> un <> sub) ga) ee
        un'             <- unify (substitute s2 t1) t2 
        return ((If c' et' ee'), substitute un' t2, un' <> s2 <> s1 <> un <> sub)
      tp -> typeError (TypeMismatch (Base Bool) tp)
  
  -- Let binding
  inferExp ga (Let bd exp) = do
    (bd', ga', sub) <- bind ga bd  
    (exp1, tp, sub') <- inferExp ga' exp 
    return (allTypes (substQType (sub' <> sub)) (Let bd' exp1), tp, sub <> sub')
  
  -- Let function expression
  inferExp ga (Recfun (Bind f term vs exp)) = do
    g1 <- bindFunc ga vs
    alpha <- fresh
    let ga' = (g1 `E.add` (f, Ty alpha))
    (exp', tp, sub) <- inferExp ga' exp 
    un <- unify (substitute sub alpha) (getFuncTy ga' sub vs tp)

    case term of 
      Nothing -> do 
        return (result, substitute un (getFuncTy ga' sub vs tp), sub <> un) 
          where result = allTypes (substQType (sub <> un)) (Recfun (Bind f (Just (Ty (substitute un (getFuncTy ga' sub vs tp)))) vs exp'))
      Just (Ty t') -> do 
        un' <- unify t' (substitute un (getFuncTy ga' sub vs tp))
        return (result, substitute un (getFuncTy ga' sub vs tp), sub <> un)   
          where result = allTypes (substQType (sub <> un)) (Recfun (Bind f (Just (Ty (substitute un (getFuncTy ga' sub vs tp)))) vs exp'))
  
  -- Case expression
  inferExp ga (Case exp [Alt "Inl" [x] exp1, Alt "Inr" [y] exp2]) = do
    (exp', tp, sub) <- inferExp ga exp
    aL <- fresh
    let gaL = ga `E.add` (x, Ty aL)
    (exp1', tpL, sub1) <- inferExp (substGamma sub gaL) exp1
    aR <- fresh
    let gaR = ga `E.add` (y, Ty aR)
    (exp2', tpR, sub2) <- inferExp (substGamma (sub1 <> sub) gaR) exp2
    un <- unify (substitute (sub2 <> sub1 <> sub) (Sum aL aR)) (substitute (sub2 <> sub1) tp)
    un' <- unify (substitute (un <> sub2) tpL) (substitute un tpR)
    return (Case exp' [Alt "Inl" [x] exp1', Alt "Inr" [y] exp2'], 
      substitute (un' <> un) tpR, un' <> un <> sub2 <> sub1 <> sub)
  
  -- Bind variables in Recfun
  bindFunc :: Gamma -> [Id] -> TC Gamma
  bindFunc ga [] = return ga
  bindFunc ga (x:xs) = do 
                      alpha <- fresh
                      bindFunc (ga `E.add` (x, Ty alpha)) xs
  
  -- get the function type
  getFuncTy :: Gamma -> Subst -> [Id] -> Type -> Type 
  getFuncTy ga sub [] tp = tp
  getFuncTy ga sub (x:xs) tp = 
    case ga `E.lookup` x of 
      Just (Ty tp1)  -> Arrow (substitute sub tp1) (getFuncTy ga sub xs tp)

  -- Bind names in let
  bind :: Gamma -> [Bind] -> TC ([Bind], Gamma, Subst)
  bind ga bd = bindHelp ga bd [] emptySubst
  
  -- Helper function of Bind
  bindHelp :: Gamma -> [Bind] -> [Bind] -> Subst -> TC ([Bind], Gamma, Subst)
  bindHelp ga [] bd sub = return (reverse bd, ga, sub)
  bindHelp ga ((Bind x ty [] exp):xs) bd sub' = do
    (exp', tp, sub)  <- inferExp ga exp 
    case ty of 
      Nothing       -> do 
        (bindHelp ga' xs ((Bind x (Just (generalise ga' tp)) [] exp'):bd) (sub <> sub'))
          where ga' =  substGamma sub (ga `E.add` (x, generalise (substGamma sub ga) tp))
      Just (Ty tp')  -> do 
        un           <- unify tp' tp
        (bindHelp ga' xs ((Bind x (Just (generalise ga' tp)) [] exp'):bd) (sub <> sub'))
          where ga' =  substGamma sub (ga `E.add` (x, generalise (substGamma sub ga) tp))
  
  