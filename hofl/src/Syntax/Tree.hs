module Syntax.Tree where

import Data.Tree

class Treeish a where
  toTree :: a -> Tree String
