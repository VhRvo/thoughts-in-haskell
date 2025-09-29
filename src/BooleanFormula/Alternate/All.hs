module BooleanFormula.Alternate.All where

import BooleanFormula.Def
import Data.Bifunctor (Bifunctor (first))
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Test.Hspec.Expectations (shouldBe)

solve :: BooleanFormula -> Maybe Env
solve formula = listToMaybe . fmap snd . filter fst $ satisfy formula Map.empty

satisfy :: BooleanFormula -> Env -> [(Bool, Env)]
satisfy formula env =
  case formula of
    Var var ->
      case Map.lookup var env of
        Nothing -> [(True, Map.insert var True env), (False, Map.insert var False env)]
        Just existed -> [(existed, env)]
    Not formula ->
      first not <$> satisfy formula env
    And lhs rhs -> do
      (result, env') <- satisfy lhs env
      if result
        then satisfy rhs env'
        else pure (False, env')
    --   (lhs', env') <- satisfy lhs env
    --   (rhs', env'') <- satisfy rhs env'
    --   pure (lhs' && rhs', env'')
    -- greedy
    Or lhs rhs -> do
      -- greedy
      (result, env') <- satisfy lhs env
      if result
        then pure (True, env')
        else satisfy rhs env'

--   (lhs', env') <- satisfy lhs env
--   (rhs', env'') <- satisfy rhs env'
--   pure (lhs' || rhs', env'')

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
  solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  -- solve test3 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
