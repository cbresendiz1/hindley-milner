module Util where

import TypeCheck
import Syntax

appExp :: Exp -> Exp -> Exp
appExp x y = (AppE x y)

lambdaExp :: Var -> Exp -> Exp
lambdaExp name ep = (AbsInfE name ep)

lEX :: String -> Exp -> Exp
lEX name ep = (AbsInfE (Var name) ep)

integerToNat :: Integer -> Exp
integerToNat n | n < 0 = error "integerToNat: negative number"
integerToNat 0 = ZeroE
integerToNat n = SuccE (integerToNat (n-1))

letF :: String -> Exp -> Exp -> Exp
letF name ex ex2 = (LetE (Var name) ex ex2)


-- let x  = let y = (\x. x) in y in (let g = (\l . l) (x (\z . g)) true)
testLet1 = letF "l"
              (letF "m"
                 (lEX "n"
                     (letF "a" (lEX "h" (VarE (Var "h")))
                           (VarE (Var "a"))
                     )
                 ) ((VarE (Var "m" )))
              )
              (VarE (Var "l"))

testRecLet = letF "l" varShort
              (letF "n" varShort (VarE (Var "l")))

testRecLetLongError = letF "l" varShort
              (letF "n" varShort
              (AppE (lEX "w"
                  (letF "m" ( AppE
                                 (AppE (VarE (Var "w")) (VarE (Var "n")))
                                 (VarE (Var "l"))
                            ) (VarE (Var "m"))
                  )
               )  TrueE))


testRecLetLong = letF "l" varShort
              (letF "n" varShort
              (AppE (lEX "w"
                  (letF "m" ( AppE
                                 (AppE (VarE (Var "w")) (VarE (Var "n")))
                                 (VarE (Var "l"))
                            ) (VarE (Var "m"))
                  )
               ) (lEX "b" (VarE (Var "b"))) ))

testLet2 = letF "l"
              (letF "m"
                 (lEX "n"
                     (letF "a" (lEX "h" (VarE (Var "h")))
                           (VarE (Var "a"))
                     )
                 ) ((VarE (Var "m" )))
              )
              (VarE (Var "m"))

testLet3 = letF "l"
              (letF "m"
                 (lEX "n"
                     (letF "a" (lEX "h" (VarE (Var "h")))
                           (VarE (Var "a"))
                     )
                 ) ((VarE (Var "m" )))
              )
              (VarE (Var "n"))

varShort =  (letF "m"
                 (lEX "n"
                     (letF "a" (lEX "h" (VarE (Var "h")))
                           (VarE (Var "a"))
                     )
                 ) ((VarE (Var "m" )))
              )

---------- March 21 -----------------------------
---- Partial application ------------------------
testLam1 = AppE
           (lEX "n"
           (lEX "m"
           (lEX "c"
           (lEX "b"
              (IfE (AppE (VarE (Var "m")) (VarE (Var "n")))
                   (VarE (Var "b"))
                   (VarE (Var "c"))
              )
           )))) (TrueE)
           
testLam3 = AppE
           (lEX "n"
           (lEX "m"
           (lEX "c"
           (lEX "b"
                  (IfE (AppE (VarE (Var "m")) (VarE (Var "n")))
                       (AppE (VarE (Var "m")) (VarE (Var "b")))
                        (VarE (Var "c"))
                  )
           )))) (TrueE)

testLam2 = AppE
           (lEX "n"
           (letF "m" (lEX "n" (VarE (Var "n")))
           (lEX "c"
           (lEX "b"
              (IfE (AppE (VarE (Var "m")) (VarE (Var "n")))
                   (VarE (Var "b"))
                   (VarE (Var "c"))
              )
           )))) (TrueE)

testIfE1 =
  (lEX "x"
      (letF "y"
          (IfE (AppE (vE "x") TrueE)
              (lEX "l" (vE "l"))
              (vE "x")
          )
      (vE "x"))
  )

testIfE2 =
  (lEX "x"
      (letF "y"
          (IfE (AppE (vE "x") TrueE)
              (AppE (lEX "l" (vE "l")) TrueE)
              (AppE (vE "y") TrueE) 
          )
      (vE "x"))
  )

testIfE3 =
  (lEX "a"
  (lEX "b"
  (lEX "c"
    (letF "x" (vE "a")
      (letF "y" (vE "b")
        (letF "z" (AppE (vE "x") (vE "y")) (vE "z")
        )
      )
    )    
  )))

testIfE4 =
  (lEX "a"
  (lEX "b"
  (lEX "c"
  (lEX "d"
    (letF "v" (vE "a")
    (letF "w" (vE "b")
    (letF "x" (vE "c")
    (letF "y" (vE "d") (AppE (AppE (vE "v") (vE "w")) (AppE (vE "y") (vE "w")))
    ))))
  ))))

vE x = (VarE (Var x))

idE x = (lEX x (VarE (Var x)))

largeIfState =
  (IfE
      (letF "x" (idE "x") (vE "x") )
      (TrueE)
      (TrueE)
  )

hm2ndTestcase = (letF "f"
                   (lEX "x"
                      (letF "g"
                              (lEX "y"
                                 (AppE (vE "x") (vE "y")))
                              (letF "foo"
                                  (AppE (vE "g") ZeroE)
                                  (SuccE (AppE (vE "g") (integerToNat 2))))))
                   --- let in f
                      (letF "foo"
                           (AppE (vE "f") (lEX "a" (integerToNat 3)))
                           (AppE (vE "f" ) (lEX "a" (integerToNat 4)))
                     )
                )

hmTestcase = (letF "f"
                 (lEX "x"
                     (letF "g"
                         (lEX "y"
                             (AppE (vE "x") (vE "y"))
                         )
                 ----------  For g
                         (letF "foo"
                             (AppE (vE "g") ZeroE)
                             (SuccE (AppE (vE "g") TrueE))
                         )
                     )
                 )
-- in (for F)                 
                 (letF "foo"
                     (AppE (vE "f") (SuccE (SuccE (SuccE ZeroE))))
                ---- For f     
                     (AppE (vE "f") TrueE)
                 )
             )

hmNormalTest = (letF "g"
                   (lEX "y"
                       (AppE (lEX "x" (vE "x")) (vE "y"))
                   )
                   (letF "foo"
                       (AppE (vE "g") (ZeroE))
                       (SuccE (AppE (vE "g") (SuccE (SuccE (ZeroE)))))
                   )
                )
               -- addExpr x y = x y

hmPolyTest = (letF "f"
                 (lEX "x" (VarE (Var "x")))
                     (letF "g" (AppE (VarE (Var "f")) TrueE)
                 (AppE (VarE (Var "f")) ZeroE))
             )

hmPolyLest = AppE (lEX "f"
                 (letF "g"
                     (AppE (vE "f") TrueE)
                 (AppE (vE "f") ZeroE)
                 )
             ) (lEX "x" (vE "x"))
--let morestuff = foldl addExpr (let "h" (lEX "c"  
