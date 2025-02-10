module RegExpr.Expr where

import Data.String
import Prelude hiding ((<>))

data Expr
  = Zero -- empty
  | One -- epsilon
  | Literal Char -- single character
  | Or Expr Expr -- union (+)
  | And Expr Expr -- concatenation (.)
  | Many Expr -- repetition (*)
  deriving (Eq, Ord, Show)

infixl 6 <+>

infixl 7 <>

(<+>) :: Expr -> Expr -> Expr
Zero <+> e = e
e <+> Zero = e
e1 <+> e2 = Or e1 e2

(<>) :: Expr -> Expr -> Expr
Zero <> _ = Zero
_ <> Zero = Zero
One <> e = e
e <> One = e
e1 <> e2 = And e1 e2

many :: Expr -> Expr
many Zero = One
many One = One
many (Many e) = Many e
many e = Many e

instance IsString Expr where
    fromString :: String -> Expr
    fromString = foldr ((<>) . Literal) One

