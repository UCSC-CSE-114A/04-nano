# Assignment 4: Nano (180 points)

## Due by Sunday 11/17, 11:59 PM


## Overview

The overall objective of this assignment is to
fully understand the notions of

* scoping,
* binding,
* environments and closures

by implementing an interpreter for a subset of Haskell.

No individual function requires more than 15-25
lines, so if you're answer is longer, you can be sure
that you need to rethink your solution.

The assignment is in the files:

1. [Eval.hs][/src/Language/Nano/Eval.hs]

and

+ [tests/Test.hs](/tests/Test.hs) has some sample tests,
  and testing code that you will use to check your
  assignments before submitting.

You should only need to modify the parts of the files which say:

```haskell
error "TBD: ..."
```

with suitable Haskell implementations.

**Note:** Start early! The next assignment builds on top of this one.


## Assignment Testing and Evaluation

Most of the points, will be awarded automatically, by
**evaluating your functions against a given test suite**.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
When you run

```shell
$ stack test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error ...` with your partial
solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

## Submission Instructions

To submit your code, push your code to gitlab and turn in the commit ID on canvas.

## Data Structures and Overview

In this assignment, you will build an interpreter
for a subset of Haskell called *Nano*. The following
data types (in `Types.hs`) are used to represent the
different elements of the language.

### Binary Operators

Nano uses the following **binary** operators encoded
within the interpreter as values of type `Binop`.

```haskell
data Binop
  = Plus
  | Minus
  | Mul
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | And
  | Or
  | Cons
```

### Expressions

All Nano programs correspond to **expressions**
each of which will be represented within your
interpreter by Haskell values of type `Expr`.

```haskell
data Expr
  = EInt  Int
  | EBool Bool
  | ENil
  | EVar Id
  | EBin Binop Expr Expr
  | EIf  Expr Expr  Expr
  | ELet Id   Expr  Expr
  | EApp Expr Expr
  | ELam Id   Expr
  deriving (Eq)
```

where `Id` is just a type alias for `String` used to represent
variable names:

```haskell
type Id = String
```

The following lists some Nano expressions,
and the value of type `Expr` used to represent
the expression inside your interpreter.

1. Let-bindings

```haskell
let x = 3 in x + x		
```

is represented by

```haskell
ELet "x" (EInt 3)
  (EBin Plus (EVar "x") (EVar "x"))
```

2. Anonymous Functions definitions

```haskell
\x -> x + 1
```

is represented by

```haskell
ELam "x" (EBin Plus (EVar "x") (EInt 1))
```

3. Function applications ("calls")

```haskell
f x									
```

is represented by

```haskell
EApp (EVar "f") (EVar "x")
```

4. (Recursive) Named Functions

```haskell
let f = \ x -> f x in
  f 5	    
```

is represented by

```haskell
ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x")))
  (EApp (Var "f") (EInt 5))
```


### Values

We will represent Nano **values**, i.e. the results
of evaluation, using the following datatype

```haskell
data Value
  = VInt  Int
  | VBool Bool
  | VClos Env Id Expr
  | VNil
  | VPair Value Value
  | VPrim (Value -> Value)
```

where an `Env` is simply a dictionary: a list of pairs
of variable names and the values they are bound to:

```haskell
type Env = [(Id, Value)]
```

Intuitively, the Nano integer value `4` and boolean value
`True` are represented respectively as `VInt 4` and `VBool True`.

- `VClos env "x" e` represents a function with argument `"x"`
   and body-expression `e` that was defined in an environment
   `env`.

## Problem 1: Nano Interpreter (Eval.hs)

In this problem, you will implement an interpreter for Nano.

### (a) 25 points


First consider the (restricted subsets of) types described below:

```haskell
data Binop = Plus | Minus | Mul

data Expr  = EInt Int  		
           | EVar Id
           | EBin Binop Expr Expr

data Value = VInt Int
```

That is,

- An *expression* is either an `Int` constant,
  a variable, or a binary operator applied
  to two sub-expressions.

- A *value* is an integer, and an *environment*
  is a list of pairs of variable names and values.

Write a Haskell function

```haskell
lookupId :: Id -> Env -> Value
```

where `lookupId x env` returns the most recent
binding for the variable `x` (i.e. the first from the left)
in the list representing the environment.
If no such value is found, you should throw an error:

```haskell
throw (Error ("unbound variable: " ++ x))
```

When you are done you should get the following behavior:

```haskell
>>> lookupId "z1" env0
0

>>> lookupId "x" env0
1

>>> lookupId "y" env0
2

>>> lookupId "mickey" env0
*** Exception: Error {errMsg = "unbound variable: mickey"}
```

Next, use `lookupId` to write a function

```haskell
eval :: Env -> Expr -> Value
```

such that `eval env e` evaluates the Nano
expression `e` in the environment `env`
(i.e. uses `env` for the values of the
**free variables** in `e`), and throws
an `Error "unbound variable"` if the
expression contains a free variable
that is **not bound** in `env`.

Once you have implemented this functionality and
recompiled, you should get the following behavior:

```haskell
>>> eval env0 (EBin Minus (EBin Plus "x" "y") (EBin Plus "z" "z1"))
0

>>> eval env0 "p"
*** Exception: Error {errMsg = "unbound variable: p"}
```

### (b) 25 points

Next, add support for the binary operators

```haskell
data Binop = ...
           | Eq | Ne | Lt | Le | And | Or
```

This will require using the new value type `Bool`

```haskell
data Value = ...
           | VBool Bool
```

* The operators `Eq` and `Ne` should work if both operands
  are `VInt` values, or if both operands are `VBool` values.  

* The operators `Lt` and `Le` are only defined for `VInt`
  values, and `&&` and `||` are only defined for `VBool`
  values.

* Other pairs of arguments are **invalid** and you should
  throw a suitable error.

```haskell
throw (Error "type error")
```

When you are done, you should see the following behavior

```haskell
>>> eval []  (EBin Le (EInt 2) (EInt 3))
True

>>> eval []  (EBin Eq (EInt 2) (EInt 3))
False

>>> eval []  (EBin Lt (EInt 2) (EBool True))
*** Exception: Error {errMsg = "type error: binop"}
```

Also note that, so long as you error message is appropriate, you will receive
points. We will not be checking for an exact error message. However,
it should contain the substring 'type error:'.

Next, implement the evaluation of `EIf p t f` expressions.  

1. First, evaluate the `p`; if `p` does not evaluate to a
   `VBool` value, then your evaluator should
   `throw (Error "type error")`,

2. If `p` evaluates to the true value then the expression
   `t` should be evaluated and returned as the value of
   the entire `If` expression,

3. Instead, if `p` evaluates to the false value, then `f`
   should be evaluated and that result should be returned.

Once you have implemented this functionality,
you should get the following behavior:

```haskell
>>> let e1 = EIf (EBin Lt "z1" "x") (EBin Ne "y" "z") (EBool False)
>>> eval env0 e1
True

>>> let e2 = EIf (EBin Eq "z1" "x") (EBin Le "y" "z") (EBin Le "z" "y")
>>> eval env0 e2
False
```

### (c) 25 points

Now consider the extended the types as shown below which includes
the *let-in* expressions which introduce local bindings.

```haskell
data Expr
  = ...
  | ELet Id   Expr  Expr
```

The expression `ELet x e1 e2` should be evaluated
as the Haskell expression `let x = e1 in e2`.

Once you have implemented this functionality and
recompiled, you should get the following behavior:

```haskell
>>> let e1 = EBin Plus "x" "y"
>>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
>>> eval [] e2
3
```

### (d)  25 points

Next, extend the evaluator so it includes the expressions
corresponding to function definitions and applications.

```haskell
data Expr
  = ...
	| ELam Id   Expr
	| EApp Expr Expr
```

In the above,

* `ELam x e` corresponds to the function defined `\x -> e`, and

* `EApp e1 e2` corresponds to the Haskell expression `e1 e2`
   (i.e. applying the argument `e2` to the function `e1`).

To evaluate functions, you will need to extend the set of
values yielded by your evaluator to include closures.

```haskell
data Value
  = ...
	| VClos Env Id Expr
```

For now, assume the functions *are not recursive*.

However, functions do have values represented by
the `VClos env x e` where

* `env` is the environment at the point where
   that function was declared,
* `x` is the formal parameter, and
* `e` the body expression of the function.  

Extend your implementation of `eval` by adding the
appropriate cases for the new type constructors.
Once you have implemented this functionality and
recompiled, you should get the following behavior:

```haskell
>>> eval [] (EApp (ELam "x" (EBin Plus "x" "x")) (EInt 3))
6

>>> let e3 = ELet "h" (ELam "y" (EBin Plus "x" "y")) (EApp "f" "h")
>>> let e2 = ELet "x" (EInt 100) e3
>>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp "g" (EInt 2)))) e2
>>> eval [] e1
102
```

### (e) 35 points

Make the above work for recursively defined functions.
Once you have implemented this functionality, you should
get the following behavior:

```haskell
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq "n" (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul "n" (EApp "fac" (EBin Minus "n" (EInt 1))))))
--             (EApp "fac" (EInt 10)))
-- :}
-- 3628800
```


### (f) 45 points

Finally, extend your program to support operations on lists.

```haskell
data Binop = ...
           | Cons

data Expr = ...
          | ENil

data Value = ...
           | VNil
           | VPair Value Value
```

In addition to the changes to the data types, add support
for two functions `head` and `tail` which do what the
corresponding Haskell functions do. Once you have implemented
this functionality and recompiled, you should get the
following behavior

```haskell
>>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)

>>> execExpr el
(1 : (2 : []))

>>> execExpr (EApp "head" el)
1

>>> execExpr (EApp "tail" el)
(2 : [])
```
The constructor `VPrim` will come in handy here.  
