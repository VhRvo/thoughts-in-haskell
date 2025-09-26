{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module BooleanFormula.LessGreedyContDefunSpecified where

import qualified Data.Map.Strict as Map
import Data.Text
import Control.Applicative ((<|>))
import Test.Hspec.Expectations (shouldBe)
import BooleanFormula.Def

solve :: BooleanFormula -> Maybe Env
solve formula = satisfyK True Map.empty formula NothingInitial JustInitial

data Just
  = JustInitial
  | JustAddTrue BooleanFormula Nothing Just
  | JustAddFalse BooleanFormula Nothing Just
  | JustOrTrue BooleanFormula Nothing Just
  | JustOrFalse BooleanFormula Nothing Just

data Nothing
  = NothingInitial
  | NothingAddFalse Env BooleanFormula Nothing Just
  | NothingOrTrue Env BooleanFormula Nothing Just

applyJust :: Just -> Env -> Maybe Env
applyJust just env =
  case just of
    JustInitial -> Just env
    JustAddTrue rhs nothing just ->
      satisfyK True env rhs nothing just
    JustAddFalse rhs nothing just ->
      satisfyK False env rhs nothing just
    JustOrTrue rhs nothing just ->
      satisfyK True env rhs nothing just
    JustOrFalse rhs nothing just ->
      satisfyK False env rhs nothing just

applyNothing :: Nothing -> Maybe Env
applyNothing =
  \case
    NothingInitial -> Nothing
    NothingAddFalse env lhs nothing just ->
      satisfyK False env lhs nothing just
    NothingOrTrue env lhs nothing just ->
      satisfyK True env lhs nothing just

satisfyK :: Bool -> Env -> BooleanFormula -> Nothing -> Just -> Maybe Env
satisfyK expected env formula nothing just =
  case formula of
    Var var ->
      case Map.lookup var env of
        Just existed
          | existed == expected -> applyJust just env
          | otherwise -> applyNothing nothing
        Nothing -> applyJust just (Map.insert var expected env)
    Not inner ->
      satisfyK (not expected) env inner nothing just
    And lhs rhs
      | expected ->
        satisfyK True env lhs nothing (JustAddTrue rhs nothing just)
      | otherwise ->
        satisfyK True env lhs
          (NothingAddFalse env lhs nothing just)
          (JustAddFalse rhs nothing just)
    Or lhs rhs
      | expected ->
        satisfyK False env lhs
          (NothingOrTrue env lhs nothing just)
          (JustOrTrue rhs nothing just)
      | otherwise ->
        satisfyK False env lhs nothing (JustOrFalse rhs nothing just)

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
--   solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test3 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
--   print (solve' test5)



