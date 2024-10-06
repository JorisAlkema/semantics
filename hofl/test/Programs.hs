-- | Pre-parsed programs for testing.
-- See the corresponding *.fl files for the source code in HOFL
module Programs where

import Syntax.Abs as Raw
import qualified DeBruijn.Abs as DB
import DeBruijn.Translate
import DeBruijn.Substitution

m, n :: DB.Exp
m = toDeBruijnTree $ Var (Ident "m")
n = toDeBruijnTree $ Var (Ident "n")

($$) :: DB.Exp -> DB.Exp -> DB.Exp
($$) = DB.App

(===) :: DB.Exp -> DB.Exp -> DB.Exp
(===) = DB.BEq

val :: Integer -> DB.Exp
val = DB.Val

idE :: DB.Exp
idE = toDeBruijnTree $
  Abstr (Lam "\\") (Ident "x") (Var (Ident "x"))

triple :: DB.Exp
triple = toDeBruijnTree $
  (Pair (Val 1) (Pair (Val 2) (Val 3)))

-- See factorial.fl
fact, factT, factRoll :: DB.Exp
fact =  toDeBruijnTree $
  Rec (Ident "f")
   (Abstr (Lam "\\") (Ident "k")
     (Ite
      (BLeq (Var (Ident "k")) (Val 0))
      (Val 1)
      (Mul
       (App (Var (Ident "f")) (Sub (Var (Ident "k")) (Val 1)))
       (Var (Ident "k"))
      )
     )
   )

factT = DB.Typed fact (TFun TInt (To "->") TInt)

factRoll = removeBinder fact fact

-- See fibonacci.fl
fib :: DB.Exp
fib = toDeBruijnTree $
  App
  (Typed
   (Rec (Ident "f")
    (Abstr (Lam "\\") (Ident "k")
     (Ite (BLeq (Var (Ident "k")) (Val 0))
      (Val 0)
      (Ite (BEq (Var (Ident "k")) (Val 1))
       (Val 1)
       (Add
        (App (Var (Ident "f")) (Sub (Var (Ident "k")) (Val 1)))
        (App (Var (Ident "f")) (Sub (Var (Ident "k")) (Val 2)))
       )
      )
     )
    )
   )
   (TFun TInt (To "->") TInt))
  (Var (Ident "n"))

-- See ackermann.fl
ackermann :: DB.Exp
ackermann = toDeBruijnTree $
  App
  (App
   (Rec (Ident "f")
    (Abstr (Lam "\\") (Ident "m")
     (Abstr (Lam "\\") (Ident "n")
      (Ite (BLeq (Var (Ident "m")) (Val 0))
       (Add (Var (Ident "n")) (Val 1))
       (Ite (BLeq (Var (Ident "n")) (Val 0))
        (App (App (Var (Ident "f")) (Sub (Var (Ident "m")) (Val 1))) (Val 1))
        (App
         (App (Var (Ident "f")) (Sub (Var (Ident "m")) (Val 1)))
         (App (App (Var (Ident "f")) (Var (Ident "m"))) (Sub (Var (Ident "n")) (Val 1)))
        )
       )
      )
     )
    )
   )
   (Var (Ident "m")))
  (Var (Ident "n"))

-- See forsome.fl
forsome :: DB.Exp
forsome = toDeBruijnTree $
  App (App (Abstr (Lam "\955") (Ident "find") (Typed (Abstr (Lam "\955") (Ident "p") (App (Var (Ident "p")) (App (Var (Ident "find")) (Var (Ident "p"))))) (TFun (TFun (TFun TInt (To "->") TBool) (To "->") TBool) (To "->") TBool))) (App (Abstr (Lam "\955") (Ident "cons") (Typed (Rec (Ident "f") (Abstr (Lam "\955") (Ident "p") (Ite (App (Var (Ident "p")) (App (App (Var (Ident "cons")) (BVal BFalse)) (App (Var (Ident "f")) (Abstr (Lam "\955") (Ident "s") (App (Var (Ident "p")) (App (App (Var (Ident "cons")) (BVal BFalse)) (Var (Ident "s")))))))) (App (App (Var (Ident "cons")) (BVal BFalse)) (App (Var (Ident "f")) (Abstr (Lam "\955") (Ident "s") (App (Var (Ident "p")) (App (App (Var (Ident "cons")) (BVal BFalse)) (Var (Ident "s"))))))) (App (App (Var (Ident "cons")) (BVal BTrue)) (App (Var (Ident "f")) (Abstr (Lam "\955") (Ident "s") (App (Var (Ident "p")) (App (App (Var (Ident "cons")) (BVal BTrue)) (Var (Ident "s")))))))))) (TFun (TFun (TFun TInt (To "->") TBool) (To "->") TBool) (To "->") (TFun TInt (To "->") TBool)))) (Typed (Abstr (Lam "\955") (Ident "x") (Abstr (Lam "\955") (Ident "s") (Abstr (Lam "\955") (Ident "i") (Ite (BLeq (Var (Ident "i")) (Val 0)) (Var (Ident "x")) (App (Var (Ident "s")) (Sub (Var (Ident "i")) (Val 1))))))) (TFun TBool (To "->") (TFun (TFun TInt (To "->") TBool) (To "->") (TFun TInt (To "->") TBool)))))) (Var (Ident "p"))
