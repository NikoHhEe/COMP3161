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
infer program = do (p',tau, s) <- runTC $ inferProgram initialGamma program
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
unify (TypeVar v1) (TypeVar v2) = 
  if v1 == v2 
    then return emptySubst
    else return (v2 =: (TypeVar v1))
unify (Base t1) (Base t2) = 
  if t1 == t2 
    then return emptySubst 
    else typeError $ TypeMismatch (Base t1) (Base t2)
unify (Prod t11 t12) (Prod t21 t22) = do
  s     <- unify t11 t21
  s'    <- unify (substitute s t12) (substitute s t22)
  return $ s <> s'
unify (Sum t11 t12) (Sum t21 t22) = do
  s     <- unify t11 t21
  s'    <- unify (substitute s t12) (substitute s t22)
  return $ s <> s'
unify (Arrow t11 t12) (Arrow t21 t22) = do
  s     <- unify t11 t21 
  s'    <- unify (substitute s t12) (substitute s t22)
  return $ s <> s'
unify (TypeVar v) t = 
  if elem v (tv t)  -- Should check if v is in the type t
    then typeError $ OccursCheckFailed v t
    else return $ v =: t
unify t (TypeVar v) = unify (TypeVar v) t
unify t1 t2 = typeError $ TypeMismatch t1 t2

generalise :: Gamma -> Type -> QType
generalise g t = foldl (\t' -> \x -> Forall x t') (Ty t) $ reverse $ filter (\x -> not $ elem x (tvGamma g)) (tv t)

-- Infer program
inferProgram :: Gamma -> Program -> TC (Program, Type, Subst)
inferProgram env [Bind name _ [] exp] = do 
  (e, t, s) <- inferExp env exp 
  case generalise env (substitute s t) of 
    Ty t  -> return ([Bind "main" (Just $ Ty (substitute s t)) [] e], substitute s t, s)
    t'    -> return ([Bind "main" (Just $ t') [] e], substitute s t, s)
inferProgram env bs = error "implement me! don't forget to run the result substitution on the entire expression using allTypes from Syntax.hs"

-- Infer expression
inferExp :: Gamma -> Exp -> TC (Exp, Type, Subst)
inferExp g (Num n) = return (Num n, Base Int, emptySubst)

-- Variables
inferExp g (Var v) = do
  case E.lookup g v of 
    Just t        -> do 
      t'  <- unquantify t 
      return (Var v, t', emptySubst)
    Nothing       -> typeError $ NoSuchVariable v 

-- Constructor types
inferExp g (Con v) = do
  case constType v of 
    Just t        -> do 
      t' <- unquantify t
      return (Con v, t', emptySubst)
    Nothing       -> typeError $ NoSuchConstructor v 

-- Prim ops
inferExp g (Prim op) = do 
  t' <- unquantify (primOpType op)
  return (Prim op, t', emptySubst)

-- Apply expression 
inferExp g (App e1 e2) = do 
  (e1', t1, s)      <- inferExp g e1
  (e2', t2, s')     <- inferExp (substGamma s g) e2
  alpha             <- fresh 
  u                 <- unify (substitute s' t1) (Arrow t2 alpha)
  return (App (allTypes (substQType (u <> s')) e1') e2', substitute u alpha, u <> s' <> s)

-- If expression
inferExp g (If e e1 e2) = do 
  (e', t, s)  <- inferExp g e
  u           <- unify t (Base Bool)
  case substitute (u <> s) t of 
    Base Bool   -> do
      (e1', t1, s1)   <- inferExp (substGamma (u <> s) g) e1
      (e2', t2, s2)   <- inferExp (substGamma (s1 <> u <> s) g) e2
      u'              <- unify (substitute s2 t1) t2 
      return ((If e' e1' e2'), substitute u' t2, u' <> s2 <> s1 <> u <> s)
    t           -> typeError $ TypeMismatch (Base Bool) t

-- Let binding
inferExp g (Let bs e) = do
  (bs', g', s)   <- bindName g bs  
  (e', t', s')   <- inferExp g' e 
  return (allTypes (substQType (s' <> s)) (Let bs' e'), t', s <> s')

-- Let function expression
inferExp g (Letfun (Bind f tau vs e)) = do
  gb              <- bindFunction g vs
  alpha           <- fresh
  let g' = (gb `E.add` (f, Ty alpha))
  (e', t, s)      <- inferExp g' e 
  u               <- unify (substitute s alpha) (getFunctionType g' s vs t)
  -- error $ 
  --   "\nt: " <> (show (substitute u $ getFunctionType g' s vs t)) <> 
  --   "\ns: " <> (show s)
  case tau of 
    Nothing       -> do 
      let out = allTypes (substQType (s <> u)) $ Letfun (Bind f (Just $ Ty $ substitute u $ getFunctionType g' s vs t) vs e')
      return (out, substitute u $ getFunctionType g' s vs t, s <> u)   
    Just (Ty t')  -> do 
      u'          <- unify t' (substitute u $ getFunctionType g' s vs t)
      let out = allTypes (substQType (s <> u)) $ Letfun (Bind f (Just $ Ty $ substitute u $ getFunctionType g' s vs t) vs e')
      return (out, substitute u $ getFunctionType g' s vs t, s <> u)   

-- Note: this is the only case you need to handle for case expressions
inferExp g (Case e [Alt "Inl" [x] e1, Alt "Inr" [y] e2]) = do
  (e', t, s)      <- inferExp g e
  alphaL          <- fresh
  let gl = g `E.add` (x, Ty alphaL)
  (e1', tl, s1)   <- inferExp (substGamma s gl) e1
  alphaR          <- fresh
  let gr = g `E.add` (y, Ty alphaR)
  (e2', tr, s2)   <- inferExp (substGamma (s1 <> s) gr) e2
  u               <- unify (substitute (s2 <> s1 <> s) (Sum alphaL alphaR)) (substitute (s2 <> s1) t)
  u'              <- unify (substitute (u <> s2) tl) (substitute u tr)
  return (Case e' [Alt "Inl" [x] e1', Alt "Inr" [y] e2'], substitute (u' <> u) tr, u' <> u <> s2 <> s1 <> s)

inferExp g (Case e _) = typeError MalformedAlternatives

-- Letrec
inferExp g (Letrec bs e) = do
  (bs', g', s)    <- bindNameRec g bs 
  (e', t', s')    <- inferExp g' e
  return (allTypes (substQType (s' <> s)) (Letrec bs' e'), t', s <> s')

-- Everything should be implemented now, so (compiler says) this is redundant
-- inferExp g _ = error "inferExp: Implement me!"

-- Add all names in binding to gamma 
bindNamesToGamma :: Gamma -> [Bind] -> TC (Gamma, [Bind])
bindNamesToGamma g bs = bindNamesToGamma' g bs []
bindNamesToGamma' :: Gamma -> [Bind] -> [Bind] -> TC (Gamma, [Bind]) 
bindNamesToGamma' g [] bs = return $ (g, bs)
bindNamesToGamma' g ((Bind x Nothing [] e):xs) bs = do
  alpha     <- fresh
  let g' = g `E.add` (x, Ty alpha)
  bindNamesToGamma' g' xs (bs ++ [Bind x (Just (Ty alpha)) [] e])
bindNamesToGamma' g ((Bind x (Just t) [] e):xs) bs = 
  bindNamesToGamma' (g `E.add` (x, t)) xs (bs ++ [Bind x (Just t) [] e])

-- Bind names for the Letrec -- Note that this has to be different from the let expression
bindNameRec :: Gamma -> [Bind] -> TC ([Bind], Gamma, Subst)
bindNameRec g bs = do 
  (g', bs')    <- bindNamesToGamma g bs
  bindNameRec' g' bs' [] emptySubst

bindNameRec' :: Gamma -> [Bind] -> [Bind] -> Subst -> TC ([Bind], Gamma, Subst)
bindNameRec' g [] bs s = return $ (reverse bs, g, s)
bindNameRec' g ((Bind x (Just (Ty t0)) [] e):xs) bs ss = do
  (e', t, s)    <- inferExp g e
  u             <- unify t t0
  let g' = substGamma s $ g `E.add` (x, generalise (substGamma s g) t)
  (bindNameRec' g' xs ((Bind x (Just (generalise g' (substitute u t))) [] e'):bs) (u <> s <> ss))

-- Bind names in let
bindName :: Gamma -> [Bind] -> TC ([Bind], Gamma, Subst)
bindName g bs = bindName' g bs [] emptySubst

bindName' :: Gamma -> [Bind] -> [Bind] -> Subst -> TC ([Bind], Gamma, Subst)
bindName' g [] bs s = return $ (reverse bs, g, s)
bindName' g ((Bind x t0 [] e):xs) bs ss = do
  (e', t, s)  <- inferExp g e 
  case t0 of 
    Nothing       -> do 
      let g' =  substGamma s $ g `E.add` (x, generalise (substGamma s g) t)
      (bindName' g' xs ((Bind x (Just (generalise g' t)) [] e'):bs) (s <> ss))
    Just (Ty t')  -> do 
      u           <- unify t' t
      let g' =  substGamma s $ g `E.add` (x, generalise (substGamma s g) t)
      (bindName' g' xs ((Bind x (Just (generalise g' t)) [] e'):bs) (s <> ss))

-- Bind variables in Letfun
bindFunction :: Gamma -> [Id] -> TC Gamma
bindFunction g [] = return $ g 
bindFunction g (x:xs) = do 
  alpha <- fresh
  bindFunction (g `E.add` (x, Ty alpha)) xs

getFunctionType :: Gamma -> Subst -> [Id] -> Type -> Type 
getFunctionType g s [] t = t
getFunctionType g s (x:xs) t = 
  case g `E.lookup` x of 
    Just (Ty t')  -> Arrow (substitute s t') (getFunctionType g s xs t)
