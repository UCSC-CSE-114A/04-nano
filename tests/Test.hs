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
              , 1
              , "1a - lookup1")
  , scoreTest ( uncurry Nano.lookupId
              ,  ("x", Nano.env0)
              , (Nano.VInt 1)
              , 1
              , "1a - lookup2")
  , failTest  ( uncurry Nano.lookupId
              , ("mickey", Nano.env0)
              , "bound"
              , 3
              , "1a - lookup3")
  , scoreTest ( uncurry3 Nano.extendEnv
              , ("x", Nano.VInt 5, Nano.env0)
              , ("x", Nano.VInt 5): Nano.env0
              , 1
              , "1a - extend"
              )
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Plus, Nano.VInt 1, Nano.VInt 2)
              , Nano.VInt 3
              , 1
              , "1b - evalOp Plus"
              )
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Minus, Nano.VInt 3, Nano.VInt 2)
              , Nano.VInt 1
              , 1
              , "1b - evalOp Minus"
              )
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Mul, Nano.VInt 2, Nano.VInt 3)
              , Nano.VInt 6
              , 1
              , "1c - evalOp Mul"
              )
  , scoreTest ( uncurry Nano.eval
              , (Nano.env0, (Nano.EBin Nano.Minus (Nano.EBin Nano.Plus "x" "y") (Nano.EBin Nano.Plus "z" "z1")))
              , (Nano.VInt 0)
              , 1
              , "1d - eval EBin 1")
  -- part 2
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Eq, Nano.VInt 1, Nano.VInt 1)
              , Nano.VBool True
              , 1
              , "2a - evalOp Eq 1 1")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Eq, Nano.VInt 1, Nano.VInt 2)
              , Nano.VBool False
              , 1
              , "2a - evalOp Eq 1 2")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Ne, Nano.VInt 1, Nano.VInt 1)
              , Nano.VBool False
              , 1
              , "2a - evalOp Ne 1 1")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Ne, Nano.VInt 1, Nano.VInt 2)
              , Nano.VBool True
              , 1
              , "2a - evalOp Ne 1 2")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Eq, Nano.VBool True, Nano.VBool True)
              , Nano.VBool True
              , 1
              , "2a - evalOp Eq True True")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Eq, Nano.VBool True, Nano.VInt 1)
              , Nano.VBool False
              , 1
              , "2a - evalOp Eq True 1")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Lt, Nano.VInt 1, Nano.VInt 2)
              , Nano.VBool True
              , 1
              , "2a - evalOp Lt 1 2")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Lt, Nano.VInt 1, Nano.VInt 1)
              , Nano.VBool False
              , 1
              , "2a - evalOp Lt 1 1")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Le, Nano.VInt 2, Nano.VInt 1)
              , Nano.VBool False
              , 1
              , "2a - evalOp Le 2 1")
  , failTest  ( uncurry3 Nano.evalOp
              , (Nano.Le, Nano.VBool True, Nano.VBool False)
              , "type error"
              , 1
              , "2a - evalOp Le True False")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.And, Nano.VBool True, Nano.VBool False)
              , Nano.VBool False
              , 1
              , "2a - evalOp And True False")
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Or, Nano.VBool True, Nano.VBool False)
              , Nano.VBool True
              , 1
              , "2a - evalOp Or True False")
  , failTest  ( uncurry3 Nano.evalOp
              , (Nano.And, Nano.VBool True, Nano.VInt 1)
              , "type error"
              , 1
              , "2a - evalOp And True 1")
  , failTest  ( uncurry3 Nano.evalOp
              , (Nano.Plus, Nano.VBool True, Nano.VInt 1)
              , "type error"
              , 1
              , "2a - evalOp Plus True 1")
  -- 2b
  , scoreTest ( uncurry Nano.eval
              , ([], Nano.EBool True)
              , Nano.VBool True
              , 1
              , "2b - eval EBool True")
  , scoreTest ( uncurry Nano.eval
              , ([], Nano.EBool False)
              , Nano.VBool False
              , 1
              , "2b - eval EBool False")
  , scoreTest ( uncurry Nano.eval
              , ([], Nano.EIf (Nano.EBool True) (Nano.EInt 2) (Nano.EBool True))
              , Nano.VInt 2
              , 3
              , "2b - eval EIf True 2 True")
  , scoreTest ( uncurry Nano.eval
              , ([], Nano.EIf (Nano.EBool False) (Nano.EInt 2) (Nano.EBool True))
              , Nano.VBool True
              , 3
              , "2b - eval EIf False 2 True")
  , failTest ( uncurry Nano.eval
              , ([], Nano.EIf (Nano.EInt 0) (Nano.EInt 2) (Nano.EBool True))
              , "type error"
              , 3
              , "2b - eval EIf 0 2 True")
  , scoreTest ( uncurry Nano.eval
              , ([],(Nano.EBin Nano.Eq (Nano.EInt 2) (Nano.EInt 3)))
              , (Nano.VBool False)
              , 5
              , "2b - eval EBin 2")
  , scoreTest ( uncurry Nano.eval
              , ([], (Nano.ELet "x" (Nano.EInt 1) (Nano.ELet "y" (Nano.EInt 2) (Nano.EBin Nano.Plus (Nano.EVar "x") (Nano.EVar "y")))))
              , Nano.VInt 3
              , 3
              , "3a - eval ELet")
  , scoreTest ( uncurry Nano.eval
              , ([], (Nano.EApp (Nano.ELam "x" (Nano.EBin Nano.Plus (Nano.EVar "x") (Nano.EVar "x"))) (Nano.EInt 3)))
              , Nano.VInt 6
              , 3
              , "3b - eval EApp")
  , scoreTest ( Nano.eval []
              , Nano.ELet "ksum" (Nano.ELam "x" (Nano.EIf (Nano.EBin Nano.Eq (Nano.EVar "x") (Nano.EInt 0)) (Nano.EInt 0) (Nano.EBin Nano.Plus (Nano.EVar "x") (Nano.EApp (Nano.EVar "ksum") (Nano.EBin Nano.Minus (Nano.EVar "x") (Nano.EInt 1)))))) (Nano.EApp (Nano.EVar "ksum") (Nano.EInt 100))
              , Nano.VInt 5050
              , 6
              , "3c - eval recursion - ksum") -- sum of first k positive integers
  -- Part 4
  -- `Cons`
  , scoreTest ( uncurry3 Nano.evalOp
              , (Nano.Cons, Nano.VInt 1, Nano.VInt 2)
              , Nano.VPair (Nano.VInt 1) (Nano.VInt 2)
              , 3
              , "4a - evalOp Cons 1 2")
  -- ENil
  , scoreTest ( uncurry Nano.eval
              , ([], Nano.ENil)
              , Nano.VNil
              , 3
              , "4a - eval ENil")
  -- `head`
  , scoreTest ( Nano.eval Nano.prelude
              , Nano.EApp (Nano.EVar "head") (Nano.EBin Nano.Cons (Nano.EInt 1) (Nano.EInt 2))
              , Nano.VInt 1
              , 3
              , "4b - eval EApp head Cons")
  , failTest  ( Nano.eval Nano.prelude
              , Nano.EApp (Nano.EVar "head") Nano.ENil
              , ""
              , 3
              , "4b - eval EApp head Nil")
  -- `tail
  , scoreTest ( Nano.eval Nano.prelude
              , Nano.EApp (Nano.EVar "tail") (Nano.EBin Nano.Cons (Nano.EInt 1) (Nano.EInt 2))
              , Nano.VInt 2
              , 3
              , "4b - eval EApp tail Cons")
  , failTest  ( Nano.eval Nano.prelude
              , Nano.EApp (Nano.EVar "tail") Nano.ENil
              , ""
              , 3
              , "4b - eval EApp tail Nil")
  -- mix
  , scoreTest ( Nano.eval Nano.prelude
              , Nano.ELet "len" (Nano.ELam "x" (Nano.EIf (Nano.EBin Nano.Eq (Nano.EVar "x") Nano.ENil) (Nano.EInt 0) (Nano.EBin Nano.Plus (Nano.EInt 1) (Nano.EApp (Nano.EVar "len") (Nano.EApp (Nano.EVar "tail") (Nano.EVar "x")))))) (Nano.EApp (Nano.EVar "len") (Nano.EBin Nano.Cons (Nano.EInt 5) (Nano.EBin Nano.Cons (Nano.EInt 5) (Nano.EBin Nano.Cons (Nano.EInt 5) (Nano.EBin Nano.Cons (Nano.EInt 5) Nano.ENil)))))
              , Nano.VInt 4
              , 6
              , "4b - recursion + VPrim, list length") -- length of a list
  , scoreTest ( uncurry Nano.eval
              , (Nano.prelude, (parse "let x = [1,2,3,4] in head (tail x)"))
              , Nano.VInt 2
              , 6
              , "4b - head of tail")
  , scoreTest ( uncurry Nano.eval
              , (Nano.prelude, (parse "let x = [1,2,3,4] in head [2,3]:tail x"))
              , Nano.valueList [Nano.VInt 2, Nano.VInt 2, Nano.VInt 3, Nano.VInt 4]
              , 6
              , "4b - head and tail")
  , scoreTest ( uncurry Nano.eval
              , (Nano.prelude, (parse "let sumList x s = if x == [] then s else sumList (tail x) (s + head x) in sumList [1,2,3,4] 0"))
              , Nano.VInt 10
              , 6
              , "4b - tail-recursive sum list") -- sum of a list
  -- integration tests
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
