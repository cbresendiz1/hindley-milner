module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))

import Parser
import Eval
import Monad
import Syntax
import TypeCheck

import Util

import TestLet
import TestLam
import TestApp

main :: IO ()
main = do
     x <- defaultMain $ testTC ++ testUnify ++ tests
     return x

tests :: [Test]
tests = map mkTestCase testCases
  where
    mkTestCase :: (String, String, Either StlcError Exp) -> Test
    mkTestCase (desc, s, result) =
        testCase desc $
        parseAndEval s @?= result


testCases :: [(String, String, Either StlcError Exp)]
testCases =
    [("true", "true", Right TrueE)
    ,("false", "false", Right FalseE)
    ,("AppE TrueE", "(\\x . x) true", Right TrueE)
    ,("AppE ZeroE", "(\\x . x) 0", Right ZeroE)
    ,("AppE ZeroE", "(\\x . x) 0", Right ZeroE)
    ,("AppE ZeroE", "(\\x . x) 0", Right ZeroE)
    ,("AppE ZeroE", "(\\x . x) (succ 0)", Right (integerToNat 1))
    ,("AppE ZeroE", "(\\x . x) (succ (succ 0))", Right (integerToNat 2))
    ,("AppE ZeroE", "(\\x . x) (succ (succ (succ 0)))", Right (integerToNat 3))
    ,("AppE ZeroE", "(\\x . x) true", Right TrueE)
    ,("AppE ZeroE", "(\\x . x) false", Right FalseE)
    ,("AppE FunT ", "(\\x . x) (\\x . x)", Right (AbsInfE (Var "x") (VarE (Var "x"))))
    ,("AppE ZeroE", "(\\x . x) (\\x . (succ 0))", Right (AbsInfE (Var "x") (SuccE ZeroE)))
    ,("AppE 0", "(\\y . (\\x. y) (succ 0)) 0", Right ZeroE)
    ,("LetE : Let test", "let xte = (\\x . x) in (let y = (xte 3) in xte y)", Right ZeroE)
    
    ,("AppE Series", "((\\y . true) ((\\x . x) 0))", Right TrueE)
    ,("AppE ZeroE Rec", "(\\y . ((\\x . x) y)) 0", Right ZeroE)
    ,("AppE TrueE | True | Rec", "(\\y . ((\\x . x) true)) true", Right TrueE)
    ,("AppE TrueE | Zero | Rec", "(\\y . ((\\x . x) true)) 0", Right TrueE)
    ,("id false", "id = (\\x . x); id false", Right FalseE)
    ,("id true", "(\\x : Bool . x) true", Right TrueE)
    ,("Non-inference pass function", "(\\x : Nat -> Nat . (x 2)) (\\y : Nat . 0)", Right ZeroE)
    ,("Tricky False", "(\\x . (\\y . false) x) 0", Right FalseE)
    ,("Inference     pass function", "(\\x . (x 2)) (\\y : Nat . 0)", Right ZeroE)
    ,("id id", "id = (\\x : Bool . x); id id", Left (TypeError (LitT BoolT) ((LitT BoolT) --> (LitT BoolT))))
    ,("if true", "if true then false else true", Right FalseE)
    ,("if false", "if false then true else false", Right FalseE)
    ,("if tricky false", "if ((\\x . (\\y . false) x) 0) then true else false", Right FalseE)

    ,("if function", "if (\\x : Bool . x) then true else false", Left (TypeError ((LitT BoolT) --> (LitT BoolT)) (LitT BoolT)))
    ,("if function", "if (\\x : Nat . x) then true else false", Left (TypeError ((LitT NatT) --> (LitT NatT)) (LitT BoolT)))
    ,("if function", "if (if 2 then 3 else false) then true else false", Left (TypeError (LitT NatT) (LitT BoolT)))
    ,("if function", "if (if false then 3 else false) then true else false", Left (TypeError (LitT NatT) (LitT BoolT)))
    ,("if function", "if (if false then 3 else false) then true else false", Left (TypeError (LitT NatT) (LitT BoolT)))
    ,("if function", "if (if true then 3 else false) then true else false", Left (TypeError (LitT NatT) (LitT BoolT)))
    ,("if Testing function", "id = false; if id then true else false", Right FalseE)

    ,("if function application", "if ((\\x : Bool . x) true) then true else false", Right TrueE)
    ,("if function application", "if ((\\x : Bool . x) 0) then true else false", Left (TypeError (LitT BoolT) (LitT NatT)))
    ,("if function application", "if ((\\x : Bool . x) true) then true else false", Right TrueE)
    ,("if function application", "if ((\\x : Bool . x) true) then true else false", Right TrueE)

    ,("applied if Bool", "(if true then (\\x : Bool .x) else (\\x : Bool .x)) true", Right TrueE)
    ,("applied if Nat", "(if true then (\\x : Nat . x) else (\\x : Nat .x)) 0", Right ZeroE)
    ,("applied if Nat", "(if ((\\x : Bool . x) true) then (\\x : Nat . x) else (\\x : Nat .x)) 0", Right ZeroE)
    -- Errors

    ,("applied if BoolF", "(if false then (\\x : Bool .x) else (\\x : Bool .x)) 0", Left (TypeError (LitT BoolT) (LitT NatT)))
    ,("applied if NatF", "(if true then (\\x : Nat .x) else (\\x : Nat .x)) true", Left (TypeError (LitT NatT) (LitT BoolT)))

    -- Not Polymorphism, just inference :)

   ,("Infer BoolT : Type Variable", "((\\x . true) true)", Right TrueE)
   ,("Infer BoolT : Type Variable", "((\\x . 0) true)", Right ZeroE)
   ,("Infer NatT  : Type Variable", "((\\x . x) 0)", Right ZeroE)
   ,("Infer BoolT : Type Variable", "((\\x . x) true)", Right TrueE)
   ,("Infer FunT : Type Variable", "((\\x . (x true)) (\\y . y))", Right TrueE)

   ,("Poly : if function", "(if ((\\x . x) true) then (\\x . x) else (\\x : Bool . true)) true ", Right TrueE)

--   ,("if function", "if (\\x : Bool . x) then true else false", Left (TypeError (LitT BoolT --> LitT BoolT) LitT BoolT))
--   ,("if function", "if (\\x : Bool . x) then true else false", Left (TypeError (LitT BoolT --> LitT BoolT) LitT BoolT))
--   ,("if function", "if (\\x : Bool . x) then true else false", Left (TypeError (LitT BoolT --> LitT BoolT) LitT BoolT))
--   ,("if function", "if (\\x : Bool . x) then true else false", Left (TypeError (LitT BoolT --> LitT BoolT) LitT BoolT))
--  ,("if function", "if (\\x : Bool . x) then true else false", Left (TypeError (LitT BoolT --> LitT BoolT) LitT BoolT))

   ,("0",
     "0",
     Right (integerToNat 0))

   ,("pred 0",
     "pred 0",
     Right (integerToNat 0))

   ,("succ 0",
     "succ 0",
     Right (integerToNat 1))

   ,("iszero 0",
     "iszero 0",
     Right TrueE)
   ]

infixr -->
(-->) :: Type -> Type -> Type
(-->) = FunT

parseAndEval :: String -> Either StlcError Exp
parseAndEval s = do
    ds <- parseStlc "v" s
    case evalDecls True ds of
      [] -> Left (OtherError "No result")
      (_, result):_ -> result

testTC :: [Test]
testTC = map mkTestCase tcTestCases
  where
    mkTestCase :: (String, (Gamma, Exp), Either StlcError (Subst, Type)) -> Test
    mkTestCase (desc, (gamma, ep), result) =
	testCase desc $
	  case (x, y) of
	    (Right (_, b), Right (_, d)) -> b @?= d		 
	    (_ , _) -> x @?= y
	    where
	      x = code2 (infer gamma ep)
	      y = result

tcTestCases :: [(String, (Gamma, Exp) , Either StlcError (Subst, Type) )]
tcTestCases =
    appCases
    ++
    lambdaCases
    ++
    letCases


testUnify :: [Test]
testUnify = map mkTestCase unTestCases
  where
    mkTestCase :: (String, (Type , Type), Either (Type, Type) Subst) -> Test
    mkTestCase (desc, (t1, t2), result) =
        testCase desc $ do
	  unify' t1 t2 @?= result


unTestCases :: [(String, (Type, Type) , Either (Type, Type) Subst)]
unTestCases =
    --------------------------------
    --------- BoolT ----------------
    --------------------------------
    [ ("Unify : BoolT" ,   (LitT BoolT , LitT BoolT), Right [] )
    , ("Unify : NatT " ,   (LitT NatT  , LitT NatT ), Right [] )

    , ("Unify : NatT  Subst Nor" ,   (VarT (TVar "a"), LitT NatT), Right [((TVar "a"), LitT NatT)] )
    , ("Unify : NatT  Subst Inv" ,   (LitT NatT, VarT (TVar "a")), Right [((TVar "a"), LitT NatT)] )

    , ("Unify : BoolT Subst Nor" , (VarT (TVar "a"), LitT BoolT), Right [((TVar "a"), LitT BoolT)] )
    , ("Unify : BoolT Subst Inv" , (LitT BoolT, VarT (TVar "a")), Right [((TVar "a"), LitT BoolT)] )
    , ("Unify : FunT Subst Inv" ,  ( FunT (LitT BoolT) (LitT BoolT)
                                     , VarT (TVar "a"))
				   , Right [((TVar "a"), FunT (LitT BoolT) (LitT BoolT))])

    , ("Unify : FunT Subst Inv" ,  ( FunT (LitT BoolT) (VarT (TVar "b"))
			             , VarT (TVar "a"))
				   , Right [((TVar "a"), FunT (LitT BoolT) (VarT (TVar "b")))])

    , ("Unify : FunT Subst Inv" ,  ( FunT (VarT (TVar "c")) (LitT BoolT)
			             , VarT (TVar "a"))
				   , Right [((TVar "a"), FunT (VarT (TVar "c")) (LitT BoolT))])

    , ("Unify : FunT Subst Inv" ,  ( FunT (VarT (TVar "c")) (VarT (TVar "b"))
			             , VarT (TVar "a"))
				   , Right [((TVar "a"), FunT (VarT (TVar "c")) (VarT (TVar "b")))])

   , ("Unify : FunT Subst Inv" ,  ( FunT (VarT (TVar "c")) (VarT (TVar "b"))
			             , FunT (VarT (TVar "d")) (VarT (TVar "e")))
				   , Right [((TVar "c"), VarT (TVar "d")),
				            ((TVar "b"), VarT (TVar "e"))])
    ]
--
--
