module TestApp where

import Syntax
import TypeCheck
import Monad
import Util

appCases :: [(String, (Gamma, Exp) , Either StlcError (Subst, Type) )]
appCases =
    --------------------------------
    --------- BoolT ----------------
    --------------------------------
    [ 
      ("App : BoolT [\\x True] infer"
    ,	([], appExp (lambdaExp (Var "x") TrueE) TrueE)
    ,	Right ([(TVar "b", LitT BoolT),(TVar "a", LitT BoolT)] :: Subst, LitT BoolT))

    , ("App : BoolT [\\x \\y True] infer"
    ,	([], appExp (lambdaExp (Var "x") TrueE)
		      (appExp (lambdaExp (Var "y") TrueE) 
				TrueE))
    ,	Right ([(TVar "d", LitT BoolT), (TVar "c", LitT BoolT), (TVar "b", LitT BoolT), (TVar "a", LitT BoolT)] :: Subst, LitT BoolT))

    , ("App : BoolT [\\x \\y y] infer"
    ,	([], appExp (lambdaExp (Var "x") (VarE (Var "x"))) 
		      (appExp (lambdaExp (Var "y") (VarE (Var "y"))) 
			      TrueE))
    ,	Right ([(TVar "e", LitT BoolT),(TVar "d", LitT BoolT), (TVar "b", LitT BoolT),(TVar "a", LitT BoolT)] :: Subst, LitT BoolT))

    , ("App : BoolT [\\x \\y x] infer"
    ,	([], appExp (lambdaExp (Var "x") 
		      (appExp (lambdaExp (Var "y") (VarE (Var "x"))) 
			      TrueE)) TrueE)
    ,	Right ([(TVar "a", LitT BoolT)] :: Subst, LitT BoolT))

    , ("App : BoolT [\\y \\x y] infer"
    ,	([], appExp (lambdaExp (Var "y") 
		      (appExp (lambdaExp (Var "x") (VarE (Var "y"))) 
			      TrueE)) TrueE)
    ,	Right ([(TVar "b", LitT BoolT), (TVar "a", LitT BoolT)] :: Subst, LitT BoolT))
    --------------------------------
    --------- NatT ----------------
    --------------------------------
    , ("App : Nat [\\x \\y y] infer"
    ,	([], appExp (lambdaExp (Var "x") (VarE (Var "x"))) 
		      (appExp (lambdaExp (Var "y") (VarE (Var "y"))) 
			      ZeroE))
    ,	Right ([(TVar "a", LitT NatT)] :: Subst, LitT NatT))

    , ("App : Nat [\\x \\y x] infer"
    ,	([], appExp (lambdaExp (Var "x") 
		      (appExp (lambdaExp (Var "y") (VarE (Var "x"))) 
			      ZeroE)) (integerToNat 3))
    ,	Right ([(TVar "a", LitT NatT)] :: Subst, LitT NatT))

    , ("App : Nat [\\y \\x y] infer"
    ,	([], appExp (lambdaExp (Var "y") 
		      (appExp (lambdaExp (Var "x") (VarE (Var "y"))) 
			      (integerToNat 5))) ZeroE)
    ,	Right ([(TVar "a", LitT NatT)] :: Subst, LitT NatT))
    ]
