module SAT.Bad where

import Data.Map.Strict (fromList, (!?))
import qualified Data.Map.Strict as Map
import Test.Hspec.Expectations
import Prelude hiding (fail, succ)
import Control.Monad.State.Strict
import SAT.Formulae

-- too greedy
-- satisfy :: BooleanFormula -> Bool -> Env -> (Bool, Env)
satisfy :: BooleanFormula -> Bool -> State Env Bool
satisfy formula expected = case formula of
    Var var -> do
        env <- get
        case env!? var of
            Nothing -> do
                put $ Map.insert var expected env
                pure True
            Just stored -> pure (stored == expected)
    Not inner -> satisfy inner (not expected)
    And left right
        | expected -> do
            result <- satisfy left True
            if result
                then satisfy right True
                else pure False
        | otherwise -> do
            result <- satisfy left False
            if result
                then pure True
                else satisfy right False
    Or left right -> satisfy (Not (And (Not left) (Not right))) expected
        -- | expected -> do
            -- result <- satisfy left True

solve :: BooleanFormula -> Maybe Env
solve formula = case runState (satisfy formula True) Map.empty of
    (result, env)
        | result -> pure env
        | otherwise -> Nothing

main :: IO ()
main = do
--   solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
--   solve test2 `shouldBe` Nothing
--   solve test3 `shouldBe` Just (Map.fromList [("a", True)])
--   solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  print (solve test5)

-- >>> main
