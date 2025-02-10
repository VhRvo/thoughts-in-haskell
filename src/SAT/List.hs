module SAT.List where

import Data.Bifunctor
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
-- import Data.Text
import System.IO.Unsafe

import SAT.Formulae
import Test.Hspec.Expectations
import Prelude hiding (fail, succ)
import Debug.Trace

satisfy :: BooleanFormula -> Env -> [(Bool, Env)]
satisfy formula env =
 case formula of
  Var var -> do
    case env !? var of
      Nothing ->
        [ (True, Map.insert var True env),
          (False, Map.insert var False env)
        ]
      Just stored -> [(stored, env)]
  Not inner -> first not <$> satisfy inner env
  And left right -> do
    (result, env') <- satisfy left env
    if result
      then satisfy right env'
      else pure (False, env')
  Or left right -> do
    (result, env') <- satisfy left env
    if result
      then pure (True, env')
      else satisfy right env'
      -- else satisfy right env

solve :: BooleanFormula -> Maybe Env
solve formula =
  case filter fst (satisfy formula Map.empty) of
    [] -> Nothing
    ((_, solution) : _) -> Just solution

solve' :: BooleanFormula -> [Env]
solve' formula = snd <$> filter fst (satisfy formula Map.empty)

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
  solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  let solutions = solve' test5
  print solutions
  print (solve' test6)
  -- print (solve' test5)
