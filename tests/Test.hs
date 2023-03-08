{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Test.Tasty
import Common
import Data.List (isInfixOf)
import qualified Language.Nano.Types as Nano
import qualified Language.Nano.Eval  as Nano

main :: IO ()
main = runTests [ unit ]

parse = Nano.parse

unit :: Score -> TestTree
unit sc = testGroup "NANO"
  [ scoreTest ( uncurry Nano.lookupId
              , ("z1", Nano.env0)
              , (Nano.VInt 0)
              , 5
              , "lookup1")
  , scoreTest ( uncurry Nano.lookupId
              ,  ("x", Nano.env0)
              , (Nano.VInt 1)
              , 5
              , "lookup2")
  , scoreTest ( uncurry Nano.eval
              , (Nano.env0, (Nano.EBin Nano.Minus (Nano.EBin Nano.Plus "x" "y") (Nano.EBin Nano.Plus "z" "z1")))
              , (Nano.VInt 0)
              , 5
              , "eval EBin 1")
  , scoreTest ( uncurry Nano.eval
              , ([],(Nano.EBin Nano.Eq (Nano.EInt 2) (Nano.EInt 3)))
              , (Nano.VBool False)
              , 5
              , "eval EBin 2")
  , scoreTest ( Nano.eval env1
              , Nano.EVar "c1"
              , Nano.VInt 1
              , 5
              , "1a - c1")
  , scoreTest ( Nano.eval env1
              , Nano.EBin Nano.Mul
                  ( Nano.EBin Nano.Minus (Nano.EInt 20) (Nano.EVar "c1"))
                  ( Nano.EBin Nano.Plus  (Nano.EVar "c2") (Nano.EVar "c3") )
              , Nano.VInt 95
              , 5
              , "1a - (20-c1)*(c2+c3)")
  , failTest  ( Nano.eval env2
              , Nano.EBin Nano.Plus (Nano.EVar "bt") (Nano.EVar "c3")
              , "type error"
              , 6
              , "1b - True + 3")
  , failTest  ( Nano.eval env2
              , Nano.EBin Nano.Or (Nano.EVar "bt") (Nano.EVar "c3")
              , "type error"
              , 6
              , "1b - bt||c3")
  , scoreTest ( Nano.eval []
              , Nano.ELet "x" (Nano.EInt 4) (Nano.EVar "x")
              , Nano.VInt 4
              , 6
              , "1c - let x = 4 in x")
  , scoreTest ( Nano.eval []
              , Nano.EApp (Nano.ELam "x" (Nano.EBin Nano.Mul (Nano.EVar "x") (Nano.EVar "x"))) (Nano.EInt 5)
              , Nano.VInt 25
              , 6
              , "1d - (fun x->x*x) 5")
  , scoreTest ( Nano.eval []
              , Nano.ELet "ksum" (Nano.ELam "x" (Nano.EIf (Nano.EBin Nano.Eq (Nano.EVar "x") (Nano.EInt 0)) (Nano.EInt 0) (Nano.EBin Nano.Plus (Nano.EVar "x") (Nano.EApp (Nano.EVar "ksum") (Nano.EBin Nano.Minus (Nano.EVar "x") (Nano.EInt 1)))))) (Nano.EApp (Nano.EVar "ksum") (Nano.EInt 100))
              , Nano.VInt 5050
              , 6
              , "1e - eval recursion - ksum") -- sum of first k positive integers
  , scoreTest ( Nano.eval Nano.prelude
              , Nano.ELet "len" (Nano.ELam "x" (Nano.EIf (Nano.EBin Nano.Eq (Nano.EVar "x") Nano.ENil) (Nano.EInt 0) (Nano.EBin Nano.Plus (Nano.EInt 1) (Nano.EApp (Nano.EVar "len") (Nano.EApp (Nano.EVar "tail") (Nano.EVar "x")))))) (Nano.EApp (Nano.EVar "len") (Nano.EBin Nano.Cons (Nano.EInt 5) (Nano.EBin Nano.Cons (Nano.EInt 5) (Nano.EBin Nano.Cons (Nano.EInt 5) (Nano.EBin Nano.Cons (Nano.EInt 5) Nano.ENil)))))
              , Nano.VInt 4
              , 6
              , "1f - recursion + VPrim, list length") -- length of a list
  , scoreTest ( uncurry Nano.eval
              , (Nano.prelude, (parse "let x = [1,2,3,4] in head (tail x)"))
              , Nano.VInt 2
              , 6
              , "1f - VPrim 1")
  , scoreTest ( uncurry Nano.eval
              , (Nano.prelude, (parse "let x = [1,2,3,4] in head [2,3]:tail x"))
              , Nano.valueList [Nano.VInt 2, Nano.VInt 2, Nano.VInt 3, Nano.VInt 4]
              , 6
              , "1f - VPrim 2")
  , scoreTest ( uncurry Nano.eval
              , (Nano.prelude, (parse "let sumList x s = if x == [] then s else sumList (tail x) (s + head x) in sumList [1,2,3,4] 0"))
              , Nano.VInt 10
              , 6
              , "1e/f - sumList tail recursion") -- sum of a list
  , fileTest  ( "tests/input/t1.hs"
              , Nano.VInt 45
              , 6 )
  , fileTest  ( "tests/input/t2.hs"
              , Nano.VInt 0
              , 6 )
  , fileTest  ( "tests/input/t3.hs"
              , Nano.VInt 2
              , 6 )
  , fileTestE ( "tests/input/t4.hs"
              , "bound"
              , 6 )
  , fileTest  ( "tests/input/t5.hs"
              , Nano.VInt 6
              , 6 )
  , fileTest  ( "tests/input/t6.hs"
              , Nano.VInt 102
              , 6 )
  , fileTest  ( "tests/input/t8.hs"
              , Nano.VInt 55
              , 6 )
  , fileTest  ( "tests/input/t9.hs"
              , Nano.VInt 3628800
              , 6 )
  , fileTest  ( "tests/input/t10.hs"
              , Nano.VInt 110
              , 6 )
  , fileTest  ( "tests/input/t11.hs"
              , Nano.VInt 55
              , 6 )
  , fileTest  ( "tests/input/t12.hs"
              , Nano.VInt 3628800
              , 6 )
  , fileTest  ( "tests/input/t13.hs"
              , Nano.VInt 80
              , 6 )
  , fileTest  ( "tests/input/t14.hs"
              , Nano.valueList [Nano.VInt 1, Nano.VInt 6, Nano.VInt 7, Nano.VInt 8]
              , 6 )
  , fileTest  ( "tests/input/t15.hs"
              , Nano.VBool False
              , 6 )
  , fileTest  ( "tests/input/t16.hs"
              , Nano.valueList [Nano.VInt 2, Nano.VInt 3, Nano.VInt 4, Nano.VInt 5]
              , 6 )
  , fileTest  ( "tests/input/t17.hs"
              , Nano.VInt 10
              , 6 )
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

    failTest :: (Show b, Eq b) => (a -> b, a, String, Int, String) -> TestTree
    failTest (f, x, err, n, msg) = scoreTest' sc (expectError err (return . f), x, True, n, msg)

    fileTest (f, r, n)  = scoreTest' sc (Nano.execFile, f, r, n, "file: " ++ f)
    fileTestE (f, e, n) = scoreTest' sc (expectError e Nano.execFile, f, True, n, "file: " ++ f)


expectError :: (Show b) => String -> (a -> IO b) -> a -> IO Bool
expectError err f x = do { r <- f x; print r; return False }
                      `catch`
                      (return . isInfixOf err . Nano.errMsg)

env1 :: Nano.Env
env1 =
  [ ("c0", Nano.VInt 0)
  , ("c1", Nano.VInt 1)
  , ("c2", Nano.VInt 2)
  , ("c3", Nano.VInt 3)
  , ("c0", Nano.VInt 4)
  , ("c1", Nano.VInt 5)
  ]

env2 :: Nano.Env
env2 = env1 ++
  [ ("bt", Nano.VBool True)
  , ("bf", Nano.VBool False)
  ]
