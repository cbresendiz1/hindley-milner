module TestLet where

import Monad
import Syntax
import TypeCheck
import Util

letCases :: [(String, (Gamma, Exp), Either StlcError (Subst, Type))]
letCases =
    [
    ("Let Bool Test"
    ,	([],  letF "x" TrueE TrueE)--(LetE (Var "x") (TrueE) TrueE) )
    ,	Right ([] :: Subst, LitT BoolT))
    
    ,  ("Let Number Test"
    ,	([],  letF "x" TrueE ZeroE)--(LetE (Var "x") (TrueE) ZeroE) )
    ,	Right ([] :: Subst, LitT NatT))

    ,  ("Let Function Test"
    ,	([],  (LetE (Var "x") (lambdaExp (Var "y") (VarE (Var "y")))
		  (AppE (VarE (Var "x"))  TrueE)
              ))
    ,	Right ([(TVar "a", LitT BoolT), (TVar "c", LitT BoolT)] :: Subst, LitT BoolT))

    ,  ("Let Function Test"
    ,	([],  (LetE (Var "x") (lambdaExp (Var "y") (VarE (Var "y")))
		  (AppE (VarE (Var "x"))  ZeroE)
		     ))
    ,	Right ([(TVar "a", LitT NatT), (TVar "c", LitT NatT)] :: Subst, LitT NatT))

    ,  ("Let Function Type Test"
    ,	([],  (LetE (Var "x") (lambdaExp (Var "y") (VarE (Var "y")))
		  (VarE (Var "x"))
		     ))
    ,	Right ([] :: Subst, FunT (VarT (TVar "b"))  (VarT (TVar "b"))))

    ,  ("Let  Test"
    ,	([],  (LetE (Var "id") (lambdaExp (Var "y") (VarE (Var "y")))
		 (AppE (lambdaExp (Var "y") (VarE (Var "y"))) TrueE)
	      ))
    ,  Right ([] :: Subst, LitT BoolT))

    ,  ("Let Basic IfE Test"
    ,	([],  (LetE (Var "id") (lambdaExp (Var "y") (VarE (Var "y")))
		  (IfE (TrueE) (ZeroE) (ZeroE))
	      ))
    ,  Right ([] :: Subst, LitT NatT))

    ,  ("Let Basic IfE Test"
    ,	([],  (LetE (Var "id") (lambdaExp (Var "y") (VarE (Var "y")))
		(LetE (Var "st") (lambdaExp (Var "j") (VarE (Var "j")))
		   (AppE (VarE (Var "st")) (VarE (Var "id")))
		)
	      ))
    ,  Right ([] :: Subst, FunT (VarT (TVar "e"))  (VarT (TVar "e"))))

    ,  ("Let IfE two variables Test"
    ,	([],  (LetE (Var "id") (lambdaExp (Var "y") (VarE (Var "y")))
		(LetE (Var "st") (lambdaExp (Var "j") (VarE (Var "j")))
		   (IfE (TrueE) (VarE (Var "st")) (VarE (Var "id")))
		)
	      ))
    ,  Right ([] :: Subst, FunT (VarT (TVar "d"))  (VarT (TVar "d"))))

    ,  ("Let IfE two variables Test"
    ,	([],  (LetE (Var "id") (lambdaExp (Var "y") (VarE (Var "y")))
		(LetE (Var "st") (lambdaExp (Var "j") (VarE (Var "j")))
		   (IfE (TrueE) (AppE (VarE (Var "id")) (VarE (Var "st"))) (VarE (Var "id")))
		)
	      ))
    ,  Right ([] :: Subst, FunT (VarT (TVar "f"))  (VarT (TVar "f"))))

    ,  ("Let Identity IfE Test"
    ,	([],  (LetE (Var "id") (lambdaExp (Var "y") (VarE (Var "y")))
		  (IfE (AppE (VarE (Var "id")) TrueE) (AppE (VarE (Var "id")) ZeroE) (AppE (VarE (Var "id")) ZeroE))
	      ))
    ,	Right ([] :: Subst, LitT NatT))

    ,  ("Let Identity IfE Test"
    ,	([],   letF "id"
               (lambdaExp (Var "y") (VarE (Var "y")))
               (IfE (AppE (VarE (Var "id")) TrueE) (AppE (VarE (Var "id")) ZeroE) (AppE (VarE (Var "id")) ZeroE))
        )
    ,	Right ([] :: Subst, LitT NatT))

    ------------------------------------
    ------------------------------------
    ------ Lambda and Let Tests --------
    ------------------------------------
    ------------------------------------

    ,  ("Special Series Test"
    ,	([], lambdaExp
             (Var "data")
             ( letF "id"
               (lambdaExp (Var "y") (VarE (Var "y")))
               (IfE (AppE (VarE (Var "data")) TrueE) (AppE (VarE (Var "id")) ZeroE) (AppE (VarE (Var "id")) ZeroE)))
             
        )
    ,	Right ([] :: Subst, FunT (FunT (LitT BoolT) (LitT BoolT))  (LitT NatT)))

    ,  ("Special Series AppE Wrong application Test"
    ,	([], AppE
             (lambdaExp
                  (Var "data")
                  ( letF "id"
                     (lambdaExp (Var "y") (VarE (Var "y")))
                     (IfE (AppE (VarE (Var "data")) TrueE) (AppE (VarE (Var "id")) ZeroE) (AppE (VarE (Var "id")) ZeroE))))
             TrueE
        )
    ,	Left $ TypeError (FunT (LitT BoolT) (LitT BoolT)) (LitT BoolT))

    ,  ("(\\data -> let id = (\\y -> y) If data (id 0) (id 0))) Test"
    ,	([], AppE
             (lambdaExp
                  (Var "data")
                  ( letF "id"
                     (lambdaExp (Var "y") (VarE (Var "y")))
                     (IfE (VarE (Var "data")) (AppE (VarE (Var "id")) ZeroE) (AppE (VarE (Var "id")) ZeroE))))
             TrueE
        )
    ,	Right ([] :: Subst, LitT NatT))

    ,  ("(\\data -> let z = 0 If true (z) (0))) Test"
    ,	([], AppE
             (lambdaExp
                  (Var "data")
                  ( letF "z"
                     (ZeroE)
                     (IfE TrueE (VarE (Var "z")) ZeroE)))
             ZeroE
        )
    ,	Right ([] :: Subst, LitT NatT))
    
    ,  ("(\\data -> let z = 0 If true (z) (0))) Test"
    ,	([], AppE
             (lambdaExp
                  (Var "data")
                  ( letF "z"
                     (ZeroE)
                     (IfE TrueE (VarE (Var "z")) ZeroE)))
             (VarE (Var "z"))
        )
    ,	Left $ NotInScope (Var "z"))

    ,  ("(\\data -> let z = 0 If true (z) (0))) Test"
    ,	([], AppE
             (lambdaExp
                  (Var "data")
                  (letF "z"
                     (ZeroE)
                     (IfE TrueE (VarE (Var "z")) ZeroE)))
             (VarE (Var "data"))
        )
    ,	Left $ NotInScope (Var "data"))

    ------------------- New Changes : March 20 ----------------------
    -- Key changes to the test cases

    , ("Testing let expressions"
    , ([], testLet1)
    , Right $ ([] :: Subst,
               FunT
                  (VarT (TVar "f"))
                  (FunT (VarT (TVar "g")) (VarT (TVar "g"))))
      )

    , ("Testing let expressions out of bounds : n"
    , ([], testRecLet)
    , Right $ ([] :: Subst,
               FunT
                  (VarT (TVar "k"))
                  (FunT (VarT (TVar "l")) (VarT (TVar "l"))))
      )

    , ("Testing let expressions : Long"
    , ([], testRecLetLong)
    , Right $ ([] :: Subst,
               FunT
                  (VarT (TVar "p"))
                  (VarT (TVar "p")))
      )

    , ("Testing let apply to true : Long"
    , ([], AppE testRecLetLong TrueE)
    , Right $ ([] :: Subst, LitT BoolT))

    ------------------------- Major Tests Above ^^^^^ -------------------

    
    , ("Testing let expressions error : n"
    , ([], testRecLetLongError)
    ,  Left $ TypeError
               (FunT (FunT (VarT (TVar "o")) (FunT (VarT (TVar "p")) (VarT (TVar "p"))))
                     (FunT
                        (FunT (VarT (TVar "q")) (FunT (VarT (TVar "r")) (VarT (TVar "r") ))
                        )
                        (VarT (TVar "m")))
               )
 
               (LitT BoolT))

    , ("Testing let expressions out of bounds : m"
    , ([], testLet2)
    , Left $ NotInScope (Var "m"))

    , ("Testing let expressions out of bounds : n"
    , ([], testLet3)
    , Left $ NotInScope (Var "n"))

    , ("This should not type error"
    , ([], hmTestcase)
    , Left $ TypeError (LitT NatT) (LitT BoolT))

    , ("This should type error : Gordon's Example"
    , ([], hm2ndTestcase)
    , Right $ ([] :: Subst, LitT NatT))

    , ("This should work : Normal let"
    , ([], hmNormalTest)
    , Right $ ([] :: Subst, LitT NatT)
      )

    , ("This should work : Normal let"
    , ([], hmPolyTest)
    , Right $ ([] :: Subst, LitT NatT)
      )

    , ("This should not work : "
    , ([], hmPolyLest)
    , Left $ TypeError (LitT BoolT) (LitT NatT)
      )      
    
    ]

