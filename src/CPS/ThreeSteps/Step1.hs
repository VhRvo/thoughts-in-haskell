module CPS.ThreeSteps.Step1 where

import Data.Text (Text)
import qualified CPS.ThreeSteps.Step0 as S0
import Control.Monad.State.Strict

data Exp
  = Trivial Trivial
  | Serious Serious

data Serious
  = Let Int Exp Exp

data Trivial
  = Var Text
  | Lam Text Exp

transform :: S0.Exp -> Exp
transform exp = evalState (go exp) 0
  where
    go ::  S0.Exp -> State Int Exp
    go = \case
      S0.Trivial exp -> Trivial <$> trivial exp
      S0.Serious exp -> Serious <$> serious exp
    trivial :: S0.Trivial -> State Int Trivial
    trivial = \case
      S0.Var var -> pure (Var var)
      S0.Lam var body -> Lam var <$> go body
    serious :: S0.Serious -> State Int Serious
    serious = \case
      S0.App fun arg -> do
        current <- get
        put (current + 1)
        Let current <$> go fun <*> go arg



