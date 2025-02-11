module PatternMatching where

import Data.Text (Text)

-- check :: Bool -> Text
-- check = \case
--     True -> "It's True"
--     False -> "It's False"

type BoolCPS r = r -> r -> r

true :: BoolCPS r
true x _ = x

false :: BoolCPS r
false _ x = x

check :: BoolCPS Text -> Text
check b = b "It's True" "It's False"

data Foobar = Zero | One Int | Two Int Int

type FoobarCPS r = r -> (Int -> r) -> (Int -> Int -> r) -> r

zero :: FoobarCPS r
zero x _ _ = x

one :: Int -> FoobarCPS r
one x _ f _ = f x

two :: Int -> Int -> FoobarCPS r
two x y _ _ f = f x y

fun :: Foobar -> Int
fun = \case
  Zero -> 0
  One a -> a + 1
  Two a b -> a + b + 2

funCPS :: FoobarCPS Int -> Int
funCPS x = x 0 (+ 1) (\a b -> a + b + 2)
