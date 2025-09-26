module BooleanFormula.Def where

import Control.Applicative ((<|>))
import Data.Map.Strict qualified as Map
import Data.Text
import Test.Hspec.Expectations (shouldBe)

type Env = Map.Map Text Bool

data BooleanFormula
  = Var Text
  | Not BooleanFormula
  | And BooleanFormula BooleanFormula
  | Or BooleanFormula BooleanFormula
  deriving (Eq, Ord, Show)

test1, test2, test3, test4, test5, test6 :: BooleanFormula
test1 = Var "a" `And` (Not (Var "b") `And` Var "c")
test2 = Var "a" `And` Not (Var "a")
test3 = Var "a" `Or` (Not (Var "b") `And` Var "c")
test4 = (Var "a" `Or` (Var "b" `Or` Var "c")) `And` (Not (Var "a") `And` Not (Var "b"))
test5 = Var "a" `And` ((Var "b" `And` Not (Var "a")) `Or` Not (Var "b"))
test6 = Var "a" `And` ((Var "b" `And` Not (Var "a")) `Or` Var "c")
