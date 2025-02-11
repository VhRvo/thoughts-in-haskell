module Datalog where

import Data.Text (Text)
import Data.Text qualified as T

type Program = [Rule]

type KnowledgeBase = [Atom]

data Rule = Rule {_head :: Atom, _body :: [Atom]}

data Atom = Atom {_predicateSymbol :: Text, _terms :: [Term]}
  deriving (Eq)

data Term
  = Variable Text
  | Symbol Text
  deriving (Eq)

type Substitution = [(Term, Term)]

emptySubstitution :: Substitution
emptySubstitution = []
