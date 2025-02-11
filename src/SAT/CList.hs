module SAT.CList where

import Data.Bifunctor (first)
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import SAT.Formulae
import Test.Hspec.Expectations (shouldBe)
import Prelude hiding (fail, head, succ, tail)

-- data Maybe a = Nothing | Just a
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- data Either e a = Left e | Right a
-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- data List a = Empty | Cons a (List a)
-- foldr :: (a -> b -> b) -> b -> List a -> b

-- satisfy :: BooleanFormula -> Env -> [(Bool, Env)]
-- satisfy :: BooleanFormula -> Env -> forall r. ((Bool, Env) -> r -> r) -> r -> r
-- satisfy :: BooleanFormula -> Env -> forall r. (Bool -> Env -> r -> r) -> r -> r
-- satisfy :: forall r. BooleanFormula -> Env -> (Bool -> Env -> r -> r) -> r -> r

newtype CList a
  = CList {unCList :: forall r. r -> (a -> r -> r) -> r}

instance Functor CList where
  fmap :: (a -> b) -> CList a -> CList b
  fmap f (CList list) =
    CList
      ( \nil cons ->
          list nil (cons . f)
      )

instance Applicative CList where
  pure :: a -> CList a
  pure x = CList (\nil cons -> cons x nil)
  (<*>) :: CList (a -> b) -> CList a -> CList b
  (<*>) (CList mf) mx =
    CList
      ( \nil cons ->
          mf
            nil
            ( \f result ->
                unCList (fmap f mx) result cons
            )
      )

instance Monad CList where
  (>>=) :: CList a -> (a -> CList b) -> CList b
  (>>=) (CList list) f =
    CList
      ( \nil cons ->
          list
            nil
            (\a r -> unCList (f a) r cons)
      )

nil' :: CList a
nil' = CList const

cons' :: a -> CList a -> CList a
cons' head (CList tail) = CList (\nil cons -> cons head (tail nil cons))

satisfy :: BooleanFormula -> Env -> CList (Bool, Env)
satisfy formula env = case formula of
  Var var -> case env !? var of
    Just stored -> pure (stored, env)
    -- succ stored env fail
    Nothing ->
      cons'
        (True, Map.insert var True env)
        (cons' (False, Map.insert var False env) nil')
  -- succ
  --   True
  --   (Map.insert var True env)
  --   (succ False (Map.insert var False env) fail)
  Not inner ->
    -- satisfy inner env (succ . not) fail
    first not <$> satisfy inner env
  -- satisfy inner env >>= (\(result, env') -> pure (not result, env'))
  And left right -> do
    -- satisfy left env
    --   >>= ( \(result, env') ->
    --           if result
    --             then satisfy right env'
    --             else pure (False, env')
    --       )
    (result, env') <- satisfy left env
    if result
      then satisfy right env'
      else pure (False, env')
  -- satisfy
  --   left
  --   env
  --   ( \result env' fail' ->
  --       if result
  --         then satisfy right env' succ fail'
  --         else succ False env' fail'
  --   )
  --   fail
  Or left right -> do
    -- satisfy left env
    --   >>= ( \(result, env') ->
    --           if result
    --             then pure (True, env')
    --             else satisfy right env'
    --       )
    (result, env') <- satisfy left env
    if result
      then pure (True, env')
      else satisfy right env'

-- satisfy
--   left
--   env
--   ( \result env' fail' ->
--       if result
--         then succ True env' fail'
--         else satisfy right env' succ fail' -- why don't use fail?
--   )
--   fail

solve :: BooleanFormula -> Maybe Env
solve bf =
  unCList
    ( satisfy
        bf
        Map.empty
    )
    Nothing
    (\(result, env) fail -> if result then Just env else fail)

-- solve' :: BooleanFormula -> [Env]
-- solve' bf =
--   satisfy
--     bf
--     Map.empty
--     (\result env fail -> if result then env : fail else fail)
--     []

main :: IO ()
main = do
  solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
  solve test2 `shouldBe` Nothing
  solve test3 `shouldBe` Just (Map.fromList [("a", True)])
  solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
  print (solve test5)
  print (solve test6)

-- print (solve' test5)

-- >>> main

integers :: CList Int
integers = cons' 1 (cons' 2 (cons' 3 nil'))

monadic :: Int -> CList Int
monadic x = cons' (x * x) (cons' (x * x + 1) nil')

integers2 :: CList Int
integers2 = integers >>= monadic

-- >>> integers 0 (+)
-- >>> integers2 0 (+)
-- 6
-- 31
