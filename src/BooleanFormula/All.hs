module BooleanFormula.All where

import qualified Data.Map.Strict as Map
import Data.Text
import Control.Applicative ((<|>))
import Test.Hspec.Expectations (shouldBe)
import BooleanFormula.Def
import Data.Maybe (listToMaybe)

solve :: BooleanFormula -> Maybe Env
solve = listToMaybe . satisfy True Map.empty

satisfy :: Bool -> Env -> BooleanFormula -> [Env]
satisfy expected env =
  \case
    Var var ->
      case Map.lookup var env of
        Just existed
          | existed == expected -> [env]
          | otherwise -> []
        Nothing -> [Map.insert var expected env]
    Not formula ->
      satisfy (not expected) env formula
    And lhs rhs
      | expected -> do
        env' <- satisfy True env lhs
        satisfy True env' rhs
      | otherwise -> do
        satisfy False env lhs <|> satisfy False env rhs
    Or lhs rhs
      | expected -> do
        satisfy True env lhs <|> satisfy True env rhs
      | otherwise -> do
        env' <- satisfy False env lhs
        satisfy False env' rhs

-- check

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
  solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  -- solve test3 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
--   print (solve' test5)



