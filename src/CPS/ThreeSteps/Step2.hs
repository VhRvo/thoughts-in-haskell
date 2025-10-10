module CPS.ThreeSteps.Step2 where

import CPS.ThreeSteps.Step1 qualified as S1
import Data.Text (Text)
import Data.Text qualified as T

data Exp
  = Trivial Trivial
  | Serious Serious

data Serious
  = Let Int Trivial Trivial Exp

data Trivial
  = Var Text
  | --   | Index Int
    Lam Text Exp

fromIndex :: Int -> Text
fromIndex = ("#" <>) . T.show

transform :: S1.Exp -> Exp
transform = \case
  S1.Trivial exp -> Trivial $ trivial exp
  S1.Serious exp -> Serious $ serious exp
  where
    trivial :: S1.Trivial -> Trivial
    trivial = \case
      S1.Var var -> Var var
      S1.Lam var body -> Lam var (transform body)
    serious :: S1.Serious -> Serious
    serious = \case
      exp@(S1.Let index _ _) -> serious' exp (Trivial (Var (fromIndex index)))
    serious' :: S1.Serious -> Exp -> Serious
    serious' exp body =
      case exp of
        S1.Let bound (S1.Trivial fun) (S1.Trivial arg) ->
          Let bound (trivial fun) (trivial arg) body
        S1.Let outer (S1.Serious fun@(S1.Let inner _ _)) (S1.Trivial arg) ->
          serious'
            fun
            ( Serious
                ( Let
                    outer
                    (Var (fromIndex inner))
                    (trivial arg)
                    body
                )
            )
        S1.Let outer (S1.Trivial fun) (S1.Serious arg@(S1.Let inner _ _)) ->
          serious'
            arg
            ( Serious
                ( Let
                    outer
                    (trivial fun)
                    (Var (fromIndex inner))
                    body
                )
            )
        S1.Let bound (S1.Serious fun@(S1.Let funIndex _ _)) (S1.Serious arg@(S1.Let argIndex _ _)) ->
          serious'
            fun
            ( Serious
                ( serious'
                    arg
                    ( Serious
                        ( Let
                            bound
                            (Var (fromIndex funIndex))
                            (Var (fromIndex argIndex))
                            body
                        )
                    )
                )
            )
