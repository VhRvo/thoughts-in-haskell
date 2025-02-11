module InvariantInCPS.Normal where

import Control.Monad.Reader
import Data.Map (Map, (!?))
import Data.Text (Text)

newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show)

type Env = Map Identifier Value

data Expr
  = Variable Identifier
  | Lambda Identifier Expr
  | Application Expr Expr
  deriving (Eq, Ord, Show)

data Value
  = VariableV Identifier
  | Closure Env Identifier Expr
  deriving (Eq, Ord, Show)

{-
func x
  | x == 0 = 17
  | x == 1 = f x
  | x == 2 = 22 + f x
  | x == 3 = g 22 (f x)
  | x == 4 = f x + 33 + g y
  | otherwise = h (f x) (44 - y) (g y)

func'k x k
  | x == 0 = k 17
  | x == 1 = f'k x k
  | x == 2 = f'k x (k . (22 +))
  | x == 3 = f'k x (\val -> g'k 22 val k)
  | x == 4 = f'k x (\val1 -> g'k y (\val2 -> k (val1 + 33 + val2)))
  | otherwise = f'k x (\val1 -> g'k y (\val2 -> k (val1 + (44 - y) ++ val2)))

func'k x
  | x == 0 = \k -> k 17
  | x == 1 = f'k x
  | x == 2 = \k -> f'k x (k . (22 +))
  | x == 3 = \k -> f'k x (\val -> g'k 22 val k)
  | x == 4 = \k -> f'k x (\val1 -> g'k y (\val2 -> k (val1 + 33 + val2)))
  | otherwise = \k -> f'k x (\val1 -> g'k y (\val2 -> k (val1 + (44 - y) ++ val2)))

-- eval :: Expr -> Reader Env Value
-- eval = \case
--   Variable Identifier
-}
