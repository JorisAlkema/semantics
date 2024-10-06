-- Haskell module generated by the BNF converter

module Syntax.Skel where

import qualified Syntax.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Syntax.Abs.Ident -> Result
transIdent x = case x of
  Syntax.Abs.Ident string -> failure x
transExp :: Syntax.Abs.Exp -> Result
transExp x = case x of
  Syntax.Abs.ENum double -> failure x
  Syntax.Abs.EVar ident -> failure x
  Syntax.Abs.ETimes exp1 exp2 -> failure x
  Syntax.Abs.EDiv exp1 exp2 -> failure x
  Syntax.Abs.EPlus exp1 exp2 -> failure x
transAssign :: Syntax.Abs.Assign -> Result
transAssign x = case x of
  Syntax.Abs.Assign ident double -> failure x
transEnv :: Syntax.Abs.Env -> Result
transEnv x = case x of
  Syntax.Abs.Env assigns -> failure x
