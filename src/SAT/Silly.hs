module SAT.Silly where

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict (Map, fromList, (!?))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import SAT.Formulae
import Test.Hspec.Expectations
import Prelude hiding (fail, succ)

-- Evaluate the formula given an environment
eval :: BooleanFormula -> ReaderT Env Maybe Bool
-- eval (Var x) = lift $ asks (Map.lookup x)
eval = \case
  Var x -> ReaderT $ Map.lookup x
  Not f -> not <$> eval f
  And f1 f2 -> (&&) <$> eval f1 <*> eval f2
  Or f1 f2 -> (||) <$> eval f1 <*> eval f2

-- Find the variables in the formula
variables :: BooleanFormula -> [Text]
variables (Var x) = [x]
variables (Not f) = variables f
variables (And f1 f2) = variables f1 <> variables f2
variables (Or f1 f2) = variables f1 <> variables f2

-- Remove duplicates from a list
unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

-- Generate all possible assignments for a set of variables
generateAssignments :: [Text] -> [Env]
generateAssignments [] = [Map.empty]
generateAssignments (x : xs) = do
  rest <- generateAssignments xs
  [Map.insert x True rest, Map.insert x False rest]

getAssignments :: BooleanFormula -> [Env]
getAssignments = generateAssignments . unique . variables

-- Solve the SAT problem
solve :: BooleanFormula -> Maybe Env
solve formula =
  let
    vars = unique (variables formula)
    assignments = generateAssignments vars
   in
    case filter (fromMaybe False . runReaderT (eval formula)) assignments of
      [] -> Nothing
      (solution : _) -> Just solution

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
  -- solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  solve test5 `shouldBe` Just (Map.fromList [("a", True), ("b", False)])

-- >>> main
