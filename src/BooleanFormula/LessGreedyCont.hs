{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
module BooleanFormula.LessGreedyCont where

import BooleanFormula.Def
import Control.Applicative ((<|>))
import Data.Map.Strict qualified as Map
import Data.Text
import Test.Hspec.Expectations (shouldBe)

solve :: BooleanFormula -> Maybe Env
solve formula = satisfyK True Map.empty formula Nothing Just

satisfyK :: Bool -> Env -> BooleanFormula -> forall b. b -> (Env -> b) -> b
satisfyK expected env formula =
  case formula of
    Var var ->
      case Map.lookup var env of
        Just existed
          | existed == expected -> \_ just -> just env
          | otherwise -> \nothing _ -> nothing
        Nothing -> \_ just -> just (Map.insert var expected env)
    Not inner ->
      satisfyK (not expected) env inner
    And lhs rhs
      | expected ->
          \nothing just ->
            satisfyK True env lhs nothing (\env' -> satisfyK True env' rhs nothing just)
      | otherwise ->
          \nothing just ->
            satisfyK True env lhs (satisfyK False env lhs nothing just) (\env' -> satisfyK False env' rhs nothing just)
    Or lhs rhs
      | expected ->
          \nothing just ->
            satisfyK False env lhs (satisfyK True env lhs nothing just) (\env' -> satisfyK True env' rhs nothing just)
      | otherwise ->
          \nothing just ->
            satisfyK False env lhs nothing (\env' -> satisfyK False env' rhs nothing just)

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
  --   solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test3 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])

--   print (solve' test5)
