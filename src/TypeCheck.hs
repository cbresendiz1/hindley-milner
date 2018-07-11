
--{ OPTIONS_GHC -fwarn-tabs #-}

module TypeCheck (
    tcExp,
    infer,
    Gamma(..),
    Subst(..),
    unify,
    code2,
    unify'
  ) where

import Control.Monad.Except

import Syntax
import Monad
import Syntax
import Control.Monad.State
import qualified Data.Map as MapT
import Data.List

-- | @t1@ is the type we got, @t2@ is the type we want
checkTypesEqual :: Type -> Type -> EvalM ()
checkTypesEqual t1 t2 | t1 == t2 = return ()
checkTypesEqual t1 t2 = throwError $ TypeError t1 t2

---------------------------------
---------------------------------
---------------------------------
----- Type Checking -------------
---------------------------------
---------------------------------

ftv :: Type -> [TVar]
ftv (VarT a) = [a]
ftv (FunT t1 t2) = ftv t1 `union` ftv t2
ftv (LitT a) = []

elms :: Gamma -> [Type]
elms x = map grb (map snd x)

grb :: TScheme -> Type
grb (Forall _ a) = a

ftvG :: Gamma -> [TVar]
ftvG env = foldr (union . ftv) [] (elms env)

general :: Gamma -> Type -> TScheme
general env typ = Forall tyXs typ
    where
      ftvT = ftv typ    
      ftvTG = ftvG env
      typeDiff = ftvT \\ ftvTG
      tyXs = map (\x -> VarT x) typeDiff
      
instant :: TScheme -> Infer Type
instant (Forall as t) = do
    as' <- mapM (const pop) as
    let az = map (\(VarT x) -> x) as
    let s = zip az as'
    return $ apply s t
    
member :: Eq a => a -> [a] -> Bool
member x [] = False
member x (y:ys)
    | x == y = True
    | otherwise = member x ys

occursCheck a t = a `member` ftv t

compose :: Subst -> Subst -> Subst
compose s1 s2 = map (applyType s1) (s2 `union` s1)

apply :: Subst -> Type -> Type
apply _ (LitT a) = (LitT a)
apply s t@(VarT (TVar a)) = case lookup (TVar a) s of
                               Just i  -> i
			       Nothing -> t
apply s (FunT t1 t2) = FunT (apply s t1) (apply s t2)

applyEnv :: Subst -> Gamma -> Gamma
applyEnv s env = map (applyTScheme s) env

applyType :: Subst -> (TVar, Type) -> (TVar, Type)
applyType s (v, t) =  (v, apply s t)

applyTScheme :: Subst -> (Var, TScheme) -> (Var, TScheme)
applyTScheme s (v, (Forall a t)) = (v, Forall a (apply s t))

------------------------------------
------------------------------------
-----                     ----------
-----     Unification     ----------
-----                     ----------
------------------------------------
------------------------------------

type Gamma = [(Var,  TScheme)] -- Holds -- all -- variable types
type Subst = [(TVar, Type)] -- The type for substitution

bind :: Type -> Type -> Either (Type, Type) Subst
bind a@(VarT b) t 
         | t == a = Right []
         | occursCheck b t = Left (a, t)
         | otherwise = Right [(b, t)]
         
unify' :: Type -> Type -> Either (Type, Type) Subst
unify' f1@(FunT l r) f2@(FunT l' r') =
    case (unify' l l') of
      Left a -> Left a
      (Right s1) -> case (unify'(apply s1 r) (apply s1 r')) of
          Left a  -> Left a
          Right s2 -> Right (s2 `compose` s1)
unify' b@(VarT a) t          =  bind b t
unify' t          b@(VarT a) =  bind b t
unify' (LitT   a) (LitT   b) | a == b = Right [] --return []
unify' t1         t2         =  Left (t1, t2)

unify :: Type -> Type -> Infer Subst
unify a b = case unify' a b of
              Left (rxa, rxb) -> throwError $ TypeError rxa rxb
              Right x         -> return x

--------------------------------------------
--------------------------------------------
---------                          ---------
---------      Type Checking       ---------
---------                          ---------
--------------------------------------------
--------------------------------------------
--------------------------------------------

--code2 :: Infer (Subst, Type) -> Either StlcError (Subst, Type)-- Infer (Subst, Type)
code2 :: Infer (Subst, Type) -> Either StlcError (Subst, Type)
code2 m = case evalState (runExceptT m) fresh of
    Left err -> Left err
    Right x  -> Right x

--tcExp :: Gamma -> Exp -> ()
tcExp :: Gamma -> Exp -> EvalM (Subst, Type)
tcExp  gamma e  = code2 $ infer gamma e

pop :: Infer Type
pop = do
    (x : xs) <- get
    put xs
    return x

infer :: Gamma -> Exp -> Infer (Subst, Type)
infer gam TrueE    = return ([], LitT BoolT)
infer gam FalseE   = return ([], LitT BoolT)
infer gam ZeroE    = return ([], LitT NatT )
infer gam (VarE x) = case lookup x gam of
                        Just b  -> do t <- instant b
                                      return ([], t)
                        Nothing -> throwError (NotInScope x)

-----------------------------------------------
-----------     Basic Types             -------
-----------------------------------------------
infer gam (AbsE name t2 exp0) = do
    let env = (name, (Forall [] t2)) : gam
    (s1, t1) <- infer env exp0
    return (s1, FunT (apply s1 t2) t1)
    
infer gam (AbsInfE name exp0) = do
    tv <- pop
    let env = (name, (Forall [] tv)) : gam
    (s1, t1) <- infer env exp0
    return (s1, FunT (apply s1 tv) t1)

infer env (AppE e1 e2) = do -- The function argument and expression should be the same type
    tv <- pop
    (r, t1) <- infer env e1
    (s, t2) <- infer (applyEnv r env) e2
    u <- unify (apply s t1) (FunT t2 tv)
    return (u `compose` s `compose` r, apply u tv)

infer env (LetE name d e) = do -- The function argument and expression should be the same type
    (r, dp) <- infer env d
    let newEnv = applyEnv r env
        t      = general newEnv dp -- type scheme
    (s, ep) <- infer ((name, t) : newEnv) e -- applyEnv : Subst -> Gamma ->  Gamma
    return (s `compose` r, ep)

infer env (IfE eB e1 e2) = do
    (e, t0) <- infer env eB
    (r, t1) <- infer env e1
    (s, t2) <- infer env e2
    t <- unify t0 (LitT BoolT)
    u <- unify t1 t2
    let t' = u `compose` t `compose` s `compose` r `compose` e
    return (t', apply u t1)
      
infer gam (SuccE e) = do
    (s1, t1) <- infer gam e
    u <- unify t1 (LitT NatT)
    case checkTypesEqual (apply u t1) (LitT NatT) of
         Left x  -> throwError $ TypeError t1 (LitT NatT)
         _       -> return (s1, t1)

infer gam (PredE e) = do
    (s1, t1) <- infer gam e
    u <- unify t1 (LitT NatT)
    case checkTypesEqual t1 (LitT NatT) of
         Left x  -> throwError $ TypeError t1 (LitT NatT)
         _       -> return (s1, (LitT NatT))

infer gam ZeroE    = return ([], (LitT NatT))
infer gam (IsZeroE e)     = do
    (s1, t1) <- infer gam e 
    case checkTypesEqual t1 (LitT NatT) of
         Left x  -> throwError $ TypeError t1 (LitT NatT)
         _       -> return (s1, (LitT BoolT))

infer _ e =
    fail $ "I don't yet know how to type check expression: " ++ show e

--pop :: StateT Gamma Type
fresh :: [Type] 
fresh = map (\x -> (VarT (TVar x))) letters 

letters :: [String] -- give me unlimited type variables (Courtesy of Stephen Diehl)
letters = [1..] >>= flip replicateM ['a'..'z']
