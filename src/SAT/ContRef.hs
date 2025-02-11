module SAT.ContRef where

import Data.Map.Strict (Map, fromList, (!?))
import Data.Map.Strict qualified as Map
import Data.Text
import SAT.Formulae
import Test.Hspec.Expectations (shouldBe)
import Prelude hiding (fail, succ)

solve :: BooleanFormula -> Maybe Env
solve formula = satisfy formula Map.empty (\b asst fail -> if b then Just asst else fail) Nothing

satisfy :: BooleanFormula -> Map Text Bool -> (Bool -> Env -> r -> r) -> r -> r
satisfy formula asst succ fail = case formula of
  Var v -> case asst !? v of
    Just b -> succ b asst fail
    Nothing ->
      let
        asstT = Map.insert v True asst
        asstF = Map.insert v False asst
        tryF = succ False asstF fail
        tryT = succ True asstT tryF
       in
        tryT
  Not formula' ->
    let succNot = succ . not
     in satisfy formula' asst succNot fail
  And l r ->
    let succAnd result asstAnd failAnd =
          if result
            then satisfy r asstAnd succ failAnd
            else succ False asstAnd failAnd
     in satisfy l asst succAnd fail
  Or l r ->
    let succOr result asstOr failOr =
          if result
            then succ True asstOr failOr
            else satisfy r asstOr succ failOr
     in satisfy l asst succOr fail

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
  solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  print (solve test5)

-- >>> main
