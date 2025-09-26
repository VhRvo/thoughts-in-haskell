{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module BooleanFormula.LessGreedyContDefun where

import qualified Data.Map.Strict as Map
import Data.Text
import Control.Applicative ((<|>))
import Test.Hspec.Expectations (shouldBe)
import BooleanFormula.Def

solve :: BooleanFormula -> Maybe Env
solve formula = satisfyK True Map.empty formula NothingInitial JustInitial

data Just b
  = JustInitial
  | JustAddTrue BooleanFormula (Nothing b) (Just b)
  | JustAddFalse BooleanFormula (Nothing b) (Just b)
  | JustOrTrue BooleanFormula (Nothing b) (Just b)
  | JustOrFalse BooleanFormula (Nothing b) (Just b)

data Nothing b
  = NothingInitial
  | NothingAddFalse Env BooleanFormula (Nothing b) (Just b)
  | NothingOrTrue Env BooleanFormula (Nothing b) (Just b)

applyJust :: forall b. Just b -> Env -> b
applyJust just env =
  case just of
    JustInitial -> undefined
    JustAddTrue rhs nothing just ->
      satisfyK True env rhs nothing just
    JustAddFalse rhs nothing just ->
      satisfyK False env rhs nothing just
    JustOrTrue rhs nothing just ->
      satisfyK True env rhs nothing just
    JustOrFalse rhs nothing just ->
      satisfyK False env rhs nothing just

applyNothing :: forall b. Nothing b -> b
applyNothing =
  \case
    NothingInitial -> undefined
    NothingAddFalse env lhs nothing just ->
      satisfyK False env lhs nothing just
    NothingOrTrue env lhs nothing just ->
      satisfyK True env lhs nothing just

satisfyK :: Bool -> Env -> BooleanFormula -> forall b. Nothing b -> Just b -> b
satisfyK expected env formula =
  case formula of
    Var var ->
      case Map.lookup var env of
        Just existed
          | existed == expected -> \_ just -> applyJust just env
          | otherwise -> \nothing _ -> applyNothing nothing
        Nothing -> \_ just -> applyJust just (Map.insert var expected env)
    Not inner ->
      satisfyK (not expected) env inner
    And lhs rhs
      | expected ->
        \nothing just ->
          satisfyK True env lhs nothing (JustAddTrue rhs nothing just)
      | otherwise ->
        \nothing just ->
          satisfyK True env lhs
            (NothingAddFalse env lhs nothing just)
            (JustAddFalse rhs nothing just)
    Or lhs rhs
      | expected ->
        \nothing just ->
        satisfyK False env lhs
          (NothingOrTrue env lhs nothing just)
          (JustOrTrue rhs nothing just)
      | otherwise ->
        \nothing just ->
          satisfyK False env lhs nothing (JustOrFalse rhs nothing just)

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
--   solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test3 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
--   print (solve' test5)



