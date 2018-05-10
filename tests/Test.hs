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
  [ scoreTest ( parse
              , "True"
              , Nano.EBool True
              , 1
              , "1a - \"true\"")
  , scoreTest ( parse
              , "123\n\t"
              , Nano.EInt 123
              , 1
              , "1a - \" 894\\n\\t\"")
  , scoreTest ( parse
              , "x"
              , Nano.EVar "x"
              , 1
              , "1a - \"Z\"")
  , scoreTest ( parse
              , "let x = 5 in x"
              , Nano.ELet "x" (Nano.EInt 5) (Nano.EVar "x")
              , 1
              , "1b - let x = 5 in x")
  , scoreTest ( parse
              , "\\x -> 5"
              , Nano.ELam "x" (Nano.EInt 5)
              , 1
              , "1b - fun x -> 5")
  , scoreTest ( parse
              , "if a then b else c"
              , Nano.EIf (Nano.EVar "a") (Nano.EVar "b") (Nano.EVar "c")
              , 1
              , "1b - if a then b else c")
  , scoreTest ( parse
              , "x + 2"
              ,  Nano.EBin Nano.Plus (Nano.EVar "x") (Nano.EInt 2)
              , 1
              , "1c - x+2")
  , scoreTest ( parse
              , "x <= 2"
              , Nano.EBin Nano.Le (Nano.EVar "x") (Nano.EInt 2)
              , 1
              , "1c - x<=2")
  , scoreTest ( parse
              , "x && 2"
              , Nano.EBin Nano.And (Nano.EVar "x") (Nano.EInt 2)
              , 1
              , "1c - x&&2")
  , scoreTest ( parse
              , "1 + ( 2 * ( 3 ))"
              , Nano.EBin Nano.Plus (Nano.EInt 1) (Nano.EBin Nano.Mul (Nano.EInt 2) (Nano.EInt 3))
              , 1
              , "1d - 1+(2*(3))")
  , scoreTest ( parse
              , "f x"
              , Nano.EApp (Nano.EVar "f") (Nano.EVar "x")
              , 1
              , "1d - f x")
  , scoreTest ( parse
              , "e : f"
              , Nano.EBin Nano.Cons (Nano.EVar "e") (Nano.EVar "f")
              , 1
              , "1e - e:f")
  , scoreTest ( Nano.eval env1
              , Nano.EVar "c1"
              , Nano.VInt 1
              , 2
              , "2a - c1")
  , scoreTest ( Nano.eval env1
              , Nano.EBin Nano.Mul
                  ( Nano.EBin Nano.Minus (Nano.EInt 20) "c1" )
                  ( Nano.EBin Nano.Plus  "c2" "c3")
              , Nano.VInt 95
              , 3
              , "2a - (20-c1)*(c2+c3)")
  , failTest  ( Nano.eval env2
              , Nano.EBin Nano.Plus "bt" "c3"
              , "type error"
              , 1
              , "2b - True + 3")
  , failTest  ( Nano.eval env2
              , Nano.EBin Nano.Or "bt" "c3"
              , "type error"
              , 1
              , "2b - bt||c3")
  , scoreTest ( Nano.eval []
              , Nano.ELet "x" (Nano.EInt 4) "x"
              , Nano.VInt 4
              , 1
              , "2c - let x = 4 in x")
  , scoreTest ( Nano.eval []
              , Nano.EApp (Nano.ELam "x" (Nano.EBin Nano.Mul "x" "x")) (Nano.EInt 5)
              , Nano.VInt 25
              , 2
              , "2d - (fun x->x*x) 5")

  , fileTest  ( "tests/input/t1.hs"
              , Nano.VInt 45
              , 1 )
  , fileTest  ( "tests/input/t2.hs"
              , Nano.VInt 0
              , 1 )
  , fileTest  ( "tests/input/t3.hs"
              , Nano.VInt 2
              , 1 )
  , fileTestE ( "tests/input/t4.hs"
              , "bound"
              , 1 )
  , fileTest  ( "tests/input/t5.hs"
              , Nano.VInt 6
              , 1 )
  , fileTest  ( "tests/input/t6.hs"
              , Nano.VInt 102
              , 2 )
  , fileTest  ( "tests/input/t8.hs"
              , Nano.VInt 55
              , 2 )
  , fileTest  ( "tests/input/t9.hs"
              , Nano.VInt 3628800
              , 2 )
  , fileTest  ( "tests/input/t10.hs"
              , Nano.VInt 110
              , 2 )
  , fileTest  ( "tests/input/t11.hs"
              , Nano.VInt 55
              , 2 )
  , fileTest  ( "tests/input/t12.hs"
              , Nano.VInt 3628800
              , 2 )
  , fileTest  ( "tests/input/t13.hs"
              , Nano.VInt 80
              , 2 )
  , fileTest  ( "tests/input/t14.hs"
              , Nano.valueList [Nano.VInt 1, Nano.VInt 6, Nano.VInt 7, Nano.VInt 8]
              , 2 )
  , fileTest  ( "tests/input/t15.hs"
              , Nano.VBool False
              , 2 )
  , fileTest  ( "tests/input/t16.hs"
              , Nano.valueList [Nano.VInt 2, Nano.VInt 3, Nano.VInt 4, Nano.VInt 5]
              , 3 )
  , fileTest  ( "tests/input/t17.hs"
              , Nano.VInt 10
              , 3 )
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
