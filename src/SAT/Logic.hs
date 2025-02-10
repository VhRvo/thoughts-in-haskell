module SAT.Logic where

import Control.Monad.State.Strict hiding (state)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
-- import Data.Text

import SAT.Formulae
import Test.Hspec.Expectations
import Prelude hiding (fail, succ)

satisfy :: BooleanFormula -> StateT Env [] Bool
satisfy = \case
  Var var -> do
    env <- get
    case env !? var of
      Nothing ->
        StateT $
          const
            [ (True, Map.insert var True env),
              (False, Map.insert var False env)
            ]
      Just stored -> pure stored
  Not inner -> not <$> satisfy inner
  And left right -> do
    result <- satisfy left
    if result
      then satisfy right
      else pure False
  Or left right -> do
    result <- satisfy left
    if result
      then pure True
      else satisfy right

solve :: BooleanFormula -> Maybe Env
solve formula =
  case filter fst (runStateT (satisfy formula) Map.empty) of
    [] -> Nothing
    ((_, solution) : _) -> Just solution

solve' :: BooleanFormula -> [Env]
solve' formula = snd <$> filter fst (runStateT (satisfy formula) Map.empty)

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
  solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  print (solve' test5)
