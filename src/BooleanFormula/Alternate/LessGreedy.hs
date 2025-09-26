{-# LANGUAGE EmptyCase #-}

module BooleanFormula.Alternate.LessGreedy where

import BooleanFormula.Def
import Data.Bifunctor (Bifunctor (first))
import Data.Map.Strict qualified as Map

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
