{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module BooleanFormula.LessGreedyContDefunSpecified3 where

import qualified Data.Map.Strict as Map
import Data.Text
import Control.Applicative ((<|>))
import Test.Hspec.Expectations (shouldBe)
import BooleanFormula.Def

solve :: BooleanFormula -> Maybe Env
solve formula = satisfyK True Map.empty formula [] []

-- Cycle in type synonym declarations:
-- type Just = [(Bool, BooleanFormula, Nothing)]
-- type Nothing = [(Bool, Env, BooleanFormula, Just)]

type Just = [JustItem]
type Nothing = [NothingItem]

data JustItem
  = JustItem Bool BooleanFormula Nothing

data NothingItem
  = NothingItem Bool Env BooleanFormula Just


applyJust :: Just -> Env -> Maybe Env
applyJust just env =
  case just of
    [] -> Just env
    JustItem boolean rhs nothing : just ->
      satisfyK boolean env rhs nothing just

applyNothing :: Nothing -> Maybe Env
applyNothing =
  \case
    [] -> Nothing
    NothingItem boolean env lhs just : nothing ->
      satisfyK boolean env lhs nothing just

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
        satisfyK True env lhs nothing (JustItem True rhs nothing : just)
      | otherwise ->
        satisfyK True env lhs
          (NothingItem False env lhs just : nothing)
          (JustItem False rhs nothing : just)
    Or lhs rhs
      | expected ->
        satisfyK False env lhs
          (NothingItem True env lhs just : nothing)
          (JustItem True rhs nothing : just)
      | otherwise ->
        satisfyK False env lhs nothing (JustItem False rhs nothing : just)

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
--   solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test3 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
--   print (solve' test5)



