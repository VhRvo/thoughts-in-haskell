module SimpleInfer.Syntax where

import Data.Text (Text)
import Data.Text qualified as T

type Identifier = Text

data Expr
  = EVariable Identifier
  | ELambda Identifier Expr
  | EApplication Expr Expr
  | ELet Identifier Expr Expr
  | ELiteral Literal
  deriving (Eq, Ord, Show)

data Literal
  = LInteger Int
  | LBoolean Bool
  deriving (Eq, Ord, Show)

-- data Type
--   = TConstant
--   | TVariable Identifier
--   | TApplication Type Type

-- data TypeF r
--   = TConstant
--   | TVariable Identifier
--   | TApplication r r

-- data UnknownF f r
--   = Known (f (UnknownF f r))
--   | Unknown Identifier

-- actually type variables is unknown values,
-- we do not know it now, but it will be known.
data Type
  = TBoolean
  | TInteger
  | TVariable Identifier
  | TArrow Type Type
  deriving (Eq, Ord, Show)

data Scheme = Scheme [Identifier] Type
  deriving (Eq, Ord, Show)
