{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module BooleanFormula.Alternate.AllCont where

import BooleanFormula.Def
import Data.Bifunctor (Bifunctor (first))
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Test.Hspec.Expectations (shouldBe)

-- solve :: BooleanFormula -> Maybe Env
-- solve formula = listToMaybe . fmap snd . filter fst $ satisfyK formula Map.empty

newtype ListC a
  = ListC (forall r. r -> (a -> r -> r) -> r)

runListC :: ListC a -> forall r. r -> (a -> r -> r) -> r
runListC (ListC cont) = cont

instance Functor ListC where
  fmap :: (a -> b) -> ListC a -> ListC b
  fmap f (ListC cont) =
    ListC (\empty cons ->
      cont empty (\element rest -> cons (f element) rest))

instance Applicative ListC where
  pure :: a -> ListC a
  pure element =
    ListC (\empty cons ->
      cons element empty)

  (<*>) :: ListC (a -> b) -> ListC a -> ListC b
  (ListC contF) <*> (ListC contX) =
    ListC (\empty cons ->
      -- ?
      contF empty (\function rest ->
        contX empty (\element rest' ->
          cons (function element) rest')
        ))

append :: ListC a -> ListC a -> ListC a
append (ListC lhs) (ListC rhs) =
  ListC (\empty cons ->
    lhs (rhs empty cons)
      (\element rest ->
        cons element rest))

instance Monad ListC where
  (>>=) :: ListC a -> (a -> ListC b) -> ListC b
  ListC cont >>= f =
    ListC (\empty cons ->
      cont empty (\element rest ->
        runListC (f element) rest cons))


-- satisfyK :: BooleanFormula -> Env -> forall r. r -> (Bool -> Env -> r -> r) -> r
-- satisfyK formula env =
--   case formula of
--     Var var ->
--       case Map.lookup var env of
--         Nothing ->
--           \empty cons ->
--           cons True (Map.insert var True env)
--             (cons False (Map.insert var False env)
--               empty)
--         Just existed -> \empty cons -> cons existed env empty
--     Not formula ->
--       \empty cons ->
--       satisfyK formula env empty (\boolean env' result -> cons (not boolean) env' result)
--     And lhs rhs ->
--       \empty cons ->
--       (do
--         (result, env') <- satisfyK lhs env
--         if result
--           then satisfyK rhs env'
--           else pure (False, env')) empty cons
--     --   (lhs', env') <- satisfyK lhs env
--     --   (rhs', env'') <- satisfyK rhs env'
--     --   pure (lhs' && rhs', env'')
--     -- greedy
--     Or lhs rhs -> do
--       -- greedy
--       (result, env') <- satisfyK lhs env
--       if result
--         then pure (True, env')
--         else satisfyK rhs env'

--   (lhs', env') <- satisfyK lhs env
--   (rhs', env'') <- satisfyK rhs env'
--   pure (lhs' || rhs', env'')

-- main :: IO ()
-- main = do
--   solve test1 `shouldBe` Just (Map.fromList [("a", True), ("b", False), ("c", True)])
--   solve test2 `shouldBe` Nothing
--   solve test3 `shouldBe` Just (Map.fromList [("a", True)])
--   -- solve test3 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
--   solve test4 `shouldBe` Just (Map.fromList [("a", False), ("b", False), ("c", True)])
