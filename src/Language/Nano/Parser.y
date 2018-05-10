{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQL _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }


-- Operators
%right in
%nonassoc '=' '==' '/=' '<' '<=' if then else
%right ':' '->'
%left '||' '&&'
%left '+' '-'
%left '*'
%%

Top  : ID '=' Expr                 { $3 }
     | Expr                        { $1 }

Expr : TNUM                        { EInt $1 }

{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
