module MinHS.TyInfer where

import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Subst
import MinHS.TCMonad

import Data.Monoid (Monoid (..), (<>))
import Data.Foldable (foldMap)
import Data.List (nub, union, (\\))
import Data.Maybe

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
unify (TypeVar a) t@(TypeVar b)
  | a == b                        = return emptySubst
  | otherwise                     = return (a =: t)
unify t1@(Base a) t2@(Base b)
  | a == b                        = return emptySubst
  | otherwise                     = typeError (TypeMismatch t1 t2)
unify (Prod x1 y1) (Prod x2 y2)   = unifyDouble (x1, y1) (x2, y2)
unify (Sum x1 y1) (Sum x2 y2)     = unifyDouble (x1, y1) (x2, y2)
unify (Arrow x1 y1) (Arrow x2 y2) = unifyDouble (x1, y1) (x2, y2)
unify (TypeVar a) t
  | occursFree a t                = typeError (OccursCheckFailed a t)
  | otherwise                     = return (a =: t)
unify t (TypeVar a)
  | occursFree a t                = typeError (OccursCheckFailed a t)
  | otherwise                     = return (a =: t)
unify t1 t2                       = typeError (TypeMismatch t1 t2)

unifyDouble :: (Type, Type) -> (Type, Type) -> TC Subst
unifyDouble (x1, y1) (x2, y2) = do s <- unify x1 x2
                                   let res1 = substitute s y1
                                       res2 = substitute s y2
                                   s' <- unify res1 res2
                                   return (s <> s')

occursFree :: Id -> Type -> Bool
occursFree a (TypeVar b)
  | a == b    = True
  | otherwise = False
occursFree a (Sum t1 t2)   = (occursFree a t1) || (occursFree a t2)
occursFree a (Prod t1 t2)  = (occursFree a t1) || (occursFree a t2)
occursFree a (Arrow t1 t2) = (occursFree a t1) || (occursFree a t2)
occursFree a (Base _)      = False

generalise :: Gamma -> Type -> QType
generalise env t = idsToQ ((tvQ (Ty t)) \\ (tvGamma env)) t
                 
idsToQ :: [Id] -> Type -> QType
idsToQ [] t = Ty t
idsToQ (id:ids) t = Forall id (idsToQ ids t)

inferProgram :: Gamma -> Program -> TC (Program, Type, Subst)
inferProgram env bs = do (Let p _, t, s) <- inferExp env (Let bs (Var "main"))
--inferProgram env bs = do (p, s, env') <- inferBinds env bs
--                         let t = fromJust (E.lookup env' "main")
--                         t' <- unquantify t
                         let p' = map (allTypesBind (substQType s)) p
                         return (p', t, s)

inferBinds :: Gamma -> [Bind] -> TC ([Bind], Subst, Gamma)
inferBinds env [Bind i _ _ e] = do (e', t, s) <- inferExp env e
                                   let env1 = substGamma s env
                                   let t' = generalise env1 t
                                   let env2 = E.add env1 (i, t')
                                   return ([Bind i (Just (t')) [] e'], s, env2)
inferBinds env ((Bind i _ _ e):bs) = do (e', t, s) <- inferExp env e
                                        let env1 = substGamma s env
                                        let t' = generalise env1 t
                                        let env2 = E.add env1 (i, t')
                                        (bs', s', env3) <- inferBinds env2 bs
                                        return ((Bind i (Just (t')) [] e'):bs', s <> s', env3)

inferExp :: Gamma -> Exp -> TC (Exp, Type, Subst)
inferExp env e@(Var id)
  | isJust res = do res' <- unquantify (fromJust res)
                    return (e, res', emptySubst)
  | otherwise  = typeError (NoSuchVariable id)
  where res = E.lookup env id
inferExp _ e@(Prim o) = do res <- unquantify (primOpType o)
                           return (e, res, emptySubst)
inferExp _ e@(Con c)
  | isJust res = do res <- unquantify (fromJust res)
                    return (e, res, emptySubst)
  | otherwise  = typeError (NoSuchConstructor c)
  where res = constType c
inferExp _ e@(Num n) = return (e, Base Int, emptySubst)
inferExp env (App e1 e2) = do a <- fresh
                              (e1', t1, s1) <- inferExp env e1
                              (e2', t2, s2) <- inferExp (substGamma s1 env) e2
                              u <- unify (substitute s2 t1) (Arrow t2 a)
                              return (App e1' e2', substitute u a, u <> s1 <> s2)
inferExp env (If e1 e2 e3) = do (e1', t1, s1) <- inferExp env e1
                                u1 <- unify t1 (Base Bool)
                                let env1 = substGamma (s1 <> u1) env
                                (e2', t2, s2) <- inferExp env1 e2
                                let env2 = substGamma (s1 <> s2 <> u1) env
                                (e3', t3, s3) <- inferExp env2 e3
                                u2 <- unify (substitute s3 t2) t3
                                let sub = s1 <> s2 <> s3 <> u1 <> u2
                                return (If e1' e2' e3', (substitute u2 t3), sub)
inferExp env (Let bs e)    = do (bs', s, env') <- inferBinds env bs
                                (e', t, s') <- inferExp env' e
                                return (Let bs' e', t, s <> s')
inferExp env (Recfun (Bind f _ [x] e)) = do a1 <- fresh
                                            a2 <- fresh
                                            let env1 = E.add (E.add env (x, Ty a1)) (f, Ty a2)
                                            (e', t, s) <- inferExp env1 e
                                            let funtype = Arrow (substitute s a1) t    
                                            u <- unify (substitute s a2) funtype
                                            let finT = substitute u funtype
                                            return (Recfun (Bind f (Just (Ty finT)) [x] e'), finT, u <> s)                         
inferExp env (Case e [Alt "Inl" [x] e1, Alt "Inr" [y] e2]) = 
  do al <- fresh
     ar <- fresh
     (e', t, s) <- inferExp env e
     let env1 = substGamma s (E.add env (x, Ty al)) 
     (e1', tl, s1) <- inferExp env1 e1
     let env2 = substGamma s1 (substGamma s (E.add env (y, Ty ar)))
     (e2', tr, s2) <- inferExp env2 e2
     u <- unify (substitute s2 (substitute s1 (substitute s (Sum al ar)))) (substitute s2 (substitute s1 t))
     u' <- unify (substitute u (substitute s2 tl)) (substitute u tr)
     let finType = substitute u' (substitute u tr)
     return (Case e' [Alt "Inl" [x] e1', Alt "Inr" [y] e2'], finType, u' <> u <> s2 <> s1 <> s)
inferExp env (Case e _) = typeError MalformedAlternatives
inferExp g e = error (show e)
-- -- Note: this is the only case you need to handle for case expressions
-- inferExp g (Case e [Alt "Inl" [x] e1, Alt "Inr" [y] e2])
-- inferExp g (Case e _) = typeError MalformedAlternatives
