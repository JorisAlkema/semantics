{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax.Base where

import Data.Data    (Data, Typeable)
import GHC.Generics (Generic)

import Syntax.Tree
import Data.Tree
import Syntax.PrintClass

-- | Negation token together with the original string provided in the program to denote it.
newtype Neg = Neg String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- | Conjunction token together with the original string provided in the program to denote it.
newtype And = And String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- | Disjunction token together with the original string provided in the program to denote it.
newtype Or = Or String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- | λ-abstraction token together with the original string provided in the program to denote it.
newtype Lam = Lam String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- | Boolean constat token together with the original string provided in the program to denote it.
data BConst = BTrue | BFalse
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- | Product type token together with the original string provided in the program to denote it.
newtype Times = Times String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- | Function symbol token together with the original string provided in the program to denote it.
newtype To = To String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- | Program types that store the original symbol used the the program.
data Type
    = TInt | TBool | TPRod Type Times Type | TFun Type To Type
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Treeish BConst where
  toTree x = case x of
    BTrue -> Node "true" []
    BFalse -> Node "false" []

instance Treeish Type where
  toTree x = case x of
    TInt -> Node "int" []
    TBool -> Node "bool" []
    TPRod ty1 _ ty2 -> Node "×" [toTree ty1, toTree ty2]
    TFun  ty1 _ ty2 -> Node "→" [toTree ty1, toTree ty2]

instance Print Neg where
  prt _ (Neg i) = doc (showString i)

instance Print And where
  prt _ (And i) = doc (showString i)

instance Print Or where
  prt _ (Or i) = doc (showString i)

instance Print Lam where
  prt _ (Lam i) = doc (showString i)

instance Print Times where
  prt _ (Times i) = doc (showString i)

instance Print To where
  prt _ (To i) = doc (showString i)

instance Print Type where
  prt i e = case e of
    TInt -> prPrec i 2 (concatD [doc (showString "int")])
    TBool -> prPrec i 2 (concatD [doc (showString "bool")])
    TPRod type_1 times type_2 -> prPrec i 1 (concatD [prt 1 type_1, prt 0 times, prt 2 type_2])
    TFun type_1 to type_2 -> prPrec i 0 (concatD [prt 1 type_1, prt 0 to, prt 0 type_2])

instance Print BConst where
  prt i e = case e of
    BTrue -> prPrec i 0 (concatD [doc (showString "true")])
    BFalse -> prPrec i 0 (concatD [doc (showString "false")])
