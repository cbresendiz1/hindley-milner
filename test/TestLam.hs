module TestLam where

import Monad
import Syntax
import TypeCheck
import Util

lambdaCases :: [(String, (Gamma, Exp), Either StlcError (Subst, Type))]
lambdaCases =
    [
    ("Lambda Test"
    ,	([], (lambdaExp (Var "x") (VarE (Var "x"))))
    ,	Right ([] :: Subst, FunT (VarT (TVar "a"))  (VarT (TVar "a"))))

    ,  ("Lambda BoolT Test"
    ,	([], (lambdaExp (Var "x") TrueE))
    ,	Right ([] :: Subst, FunT (VarT (TVar "a"))  (LitT BoolT)))

    ,  ("Lambda NatT Test"
    ,	([], (lambdaExp (Var "x") (integerToNat 1)))
    ,	Right ([] :: Subst, FunT (VarT (TVar "a"))  (LitT NatT)))
--------------------- March 21 ----------------------------------------
    ,  ("Lambda If with lambda Test"
    ,	([], testLam1)
    ,	Right ([] :: Subst,
              FunT
                  (FunT (LitT BoolT) (LitT BoolT))
                  (FunT (VarT (TVar "d"))
                        (FunT (VarT (TVar "d"))
                              (VarT (TVar "d"))
                        ))
                  ))

    ,  ("Lambda If with Adding Let"
    ,	([], testLam1)
    ,	Right ([] :: Subst,
              FunT
                  (FunT (LitT BoolT) (LitT BoolT))
                  (FunT (VarT (TVar "d"))
                        (FunT (VarT (TVar "d"))
                              (VarT (TVar "d"))
                        ))
                  ))
    ,  ("Lambda If with Adding Identity"
    ,	([], testLam3)
    ,	Right ([] :: Subst,
              FunT
                  (FunT (LitT BoolT) (LitT BoolT))
                  (FunT (VarT (TVar "d"))
                        (FunT (VarT (TVar "e"))
                              (VarT (TVar "d"))
                        ))
                  ))       
    ]
    ++
    [
    ("Variable Test"
    ,	([(Var "x", Forall [] (LitT BoolT))], VarE (Var "x"))
    ,	Right ([] :: Subst, LitT BoolT))

    ,  ("Particular 0 case Test"
    ,  ( [], (appExp (lambdaExp (Var "x") (TrueE)) TrueE))
    ,	Right ([(TVar "b", LitT BoolT),(TVar "a", LitT BoolT)] :: Subst, LitT BoolT))

    ,  ("Particular 1 case Test"
    ,	([], (appExp 
	       (lambdaExp (Var "y")
		      (appExp (lambdaExp (Var "x") TrueE) TrueE))
		      ZeroE))
    ,	Right ([] :: Subst, LitT BoolT))


    ,  ("Particular 2 case Test"
    ,	([], (appExp 
	       (lambdaExp (Var "y") 
		      (appExp (lambdaExp (Var "x") (VarE (Var "y"))) TrueE)) 
	       ZeroE))
    ,	Right ([] :: Subst, LitT NatT))

    ,  ("TestIfValues"
    ,	([], testIfE1)
    ,	Right ([] :: Subst,
               FunT
                   (FunT (LitT BoolT)
                        (LitT BoolT))
                   (FunT (LitT BoolT)
                        (LitT BoolT))))
    ,  ("Test If 2"
    ,	([], testIfE3)
    ,	Right ([] :: Subst,
               FunT
                   (FunT (VarT (TVar "b")) (VarT (TVar "d")))
                   (FunT (VarT (TVar "b")) (FunT (VarT (TVar "c")) (VarT (TVar "d"))))
              ))

    ,  ("Test If 2"
    ,	([], testIfE3)
    ,	Right ([] :: Subst,
               FunT
                   (FunT (VarT (TVar "b")) (VarT (TVar "d")))
                   (FunT (VarT (TVar "b")) (FunT (VarT (TVar "c")) (VarT (TVar "d"))))))
    ,  ("Test If 3"
    ,	([], testIfE3)
    ,	Right ([] :: Subst,
               FunT
                   (FunT (VarT (TVar "b")) (VarT (TVar "d")))
                   (FunT (VarT (TVar "b")) (FunT (VarT (TVar "c")) (VarT (TVar "d"))))))
    ,  ("Test If 4"
    ,	([], testIfE4)
    ,	Right ([] :: Subst,
      FunT
        (FunT (VarT (TVar "b"))
             (FunT (VarT (TVar "g")) (VarT (TVar "e"))))
        (FunT (VarT (TVar "b"))
           (FunT (VarT (TVar "c"))
              (FunT (FunT (VarT (TVar "b")) (VarT (TVar "g"))) (VarT (TVar "e")))
           ))))


    
    ]
