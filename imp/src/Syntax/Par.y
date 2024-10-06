-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Syntax.Par where
import Syntax.Abs
import Syntax.Lex
import Syntax.ErrM

}

%name pCom Com
%name pEnv Env
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  ',' { PT _ (TS _ 5) }
  '-' { PT _ (TS _ 6) }
  ':=' { PT _ (TS _ 7) }
  ';' { PT _ (TS _ 8) }
  '<=' { PT _ (TS _ 9) }
  '=' { PT _ (TS _ 10) }
  '==' { PT _ (TS _ 11) }
  'do' { PT _ (TS _ 12) }
  'else' { PT _ (TS _ 13) }
  'false' { PT _ (TS _ 14) }
  'if' { PT _ (TS _ 15) }
  'skip' { PT _ (TS _ 16) }
  'then' { PT _ (TS _ 17) }
  'true' { PT _ (TS _ 18) }
  'while' { PT _ (TS _ 19) }
  L_integ  { PT _ (TI $$) }
  L_ident  { PT _ (TV $$) }
  L_Neg { PT _ (T_Neg $$) }
  L_And { PT _ (T_And $$) }
  L_Or { PT _ (T_Or $$) }

%%

Integer :: { Integer }
Integer  : L_integ  { (read ( $1)) :: Integer }

Ident   :: { Ident }
Ident    : L_ident  { Ident $1 }

Neg :: { Neg}
Neg  : L_Neg { Neg ($1)}

And :: { And}
And  : L_And { And ($1)}

Or :: { Or}
Or  : L_Or { Or ($1)}

AExp2 :: { AExp }
AExp2 : Integer { Syntax.Abs.EInt $1 }
      | Ident { Syntax.Abs.EVar $1 }
      | '(' AExp ')' { $2 }
AExp1 :: { AExp }
AExp1 : AExp1 '*' AExp2 { Syntax.Abs.ETimes $1 $3 } | AExp2 { $1 }
AExp :: { AExp }
AExp : AExp '+' AExp1 { Syntax.Abs.EPlus $1 $3 }
     | AExp '-' AExp1 { Syntax.Abs.ESub $1 $3 }
     | AExp1 { $1 }
BVal :: { BVal }
BVal : 'true' { Syntax.Abs.BTrue } | 'false' { Syntax.Abs.BFalse }
BExp3 :: { BExp }
BExp3 : BVal { Syntax.Abs.EVal $1 }
      | AExp '==' AExp { Syntax.Abs.EEq $1 $3 }
      | AExp '<=' AExp { Syntax.Abs.ELeq $1 $3 }
      | '(' BExp ')' { $2 }
BExp2 :: { BExp }
BExp2 : Neg BExp3 { Syntax.Abs.ENeg $1 $2 } | BExp3 { $1 }
BExp1 :: { BExp }
BExp1 : BExp1 And BExp2 { Syntax.Abs.EAnd $1 $2 $3 } | BExp2 { $1 }
BExp :: { BExp }
BExp : BExp Or BExp1 { Syntax.Abs.AOr $1 $2 $3 } | BExp1 { $1 }
Com2 :: { Com }
Com2 : 'skip' { Syntax.Abs.CSkip }
     | Ident ':=' AExp { Syntax.Abs.CAssign $1 $3 }
     | '(' Com ')' { $2 }
Com1 :: { Com }
Com1 : Com2 ';' Com1 { Syntax.Abs.CSeq $1 $3 } | Com2 { $1 }
Com :: { Com }
Com : 'if' BExp 'then' Com1 'else' Com { Syntax.Abs.CIte $2 $4 $6 }
    | 'while' BExp 'do' Com { Syntax.Abs.CWhile $2 $4 }
    | Com1 { $1 }
Assign :: { Assign }
Assign : Ident '=' Integer { Syntax.Abs.Assign $1 $3 }
ListAssign :: { [Assign] }
ListAssign : {- empty -} { [] }
           | Assign { (:[]) $1 }
           | Assign ',' ListAssign { (:) $1 $3 }
Env :: { Env }
Env : ListAssign { Syntax.Abs.Env $1 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}

