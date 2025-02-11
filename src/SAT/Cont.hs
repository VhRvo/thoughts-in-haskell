module SAT.Cont where

import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import SAT.Formulae
import Test.Hspec.Expectations (shouldBe)
import Prelude hiding (fail, head, succ, tail)

satisfy :: BooleanFormula -> Env -> (Bool -> Env -> r -> r) -> r -> r
satisfy formula env succ fail = case formula of
  Var var -> case env !? var of
    Just stored -> succ stored env fail
    Nothing ->
      succ
        True
        (Map.insert var True env)
        (succ False (Map.insert var False env) fail)
  Not inner -> satisfy inner env (succ . not) fail
  And left right ->
    satisfy
      left
      env
      ( \result env' fail' ->
          if result
            then satisfy right env' succ fail'
            else succ False env' fail'
      )
      fail
  Or left right ->
    satisfy
      left
      env
      ( \result env' fail' ->
          if result
            then succ True env' fail'
            else satisfy right env' succ fail' -- why don't use fail?
      )
      fail

solve :: BooleanFormula -> Maybe Env
solve bf =
  satisfy
    bf
    Map.empty
    (\result env fail -> if result then Just env else fail)
    Nothing

solve' :: BooleanFormula -> [Env]
solve' bf =
  satisfy
    bf
    Map.empty
    (\result env fail -> if result then env : fail else fail)
    []

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
  solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  print (solve' test5)

type List a = forall r. r -> (a -> r -> r) -> r

nil' :: List a
nil' nil _ = nil

cons' :: a -> List a -> List a
cons' head tail nil cons = cons head (tail nil cons)

map :: (a -> b) -> List a -> List b
map f list nil cons = list nil (cons . f)

singleton :: a -> List a
singleton x nil cons = cons x nil

bind :: List a -> (a -> List b) -> List b
bind list f nil cons = list nil (\a r -> f a r cons)

integers :: List Int
integers = cons' 1 (cons' 2 (cons' 3 nil'))

monadic :: Int -> List Int
monadic x = cons' (x * x) (cons' (x * x + 1) nil')

integers2 :: List Int
integers2 = bind integers monadic

-- >>> integers 0 (+)
-- >>> integers2 0 (+)
-- 6
-- 31
