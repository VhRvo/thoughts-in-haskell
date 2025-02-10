module SAT.Formulae where

import Data.Text (Text)

import qualified Data.Map.Strict as Map

type Env = Map.Map Text Bool

data BooleanFormula
  = Var Text
  | Not BooleanFormula
  | And BooleanFormula BooleanFormula
  | Or BooleanFormula BooleanFormula

test1, test2, test3, test4, test5, test6 :: BooleanFormula
test1 = Var "a" `And` (Not (Var "b") `And` Var "c")
test2 = Var "a" `And` Not (Var "a")
test3 = Var "a" `Or` (Not (Var "b") `And` Var "c")
test4 = (Var "a" `Or` (Var "b" `Or` Var "c")) `And` (Not (Var "a") `And` Not (Var "b"))
test5 = Var "a" `And` ((Var "b" `And` Not (Var "a")) `Or` Not (Var "b"))
test6 = Var "a" `And` ((Var "b" `And` Not (Var "a")) `Or` Var "c")

-- main :: IO ()
-- main = do
--   solve test1 `shouldBe` Just (fromList [("a", True), ("b", False), ("c", True)])
--   solve test2 `shouldBe` Nothing
--   solve test3 `shouldBe` Just (fromList [("a", True)])
--   solve test4 `shouldBe` Just (fromList [("a", False), ("b", False), ("c", True)])

