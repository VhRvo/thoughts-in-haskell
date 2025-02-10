module CPS.Normal2 where

import Data.Text (Text)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Prelude as P

newtype Identifier = Identifier Text
    deriving (Eq, Ord, Show)

data Expr
    = Constant Int
    | Add Expr Expr
    | Variable Identifier
    | Lambda Identifier Expr
    | Application Expr Expr

data Value
    = Integer Int
    | Closure (Value -> (Value -> Value) -> Value)

type Env = Map Identifier Value

add :: Value -> Value -> Value
add (Integer a) (Integer b) = Integer (a + b)
add _ _ = error "add: invalid arguments"

apply :: Value -> Value -> (Value -> Value) -> Value
-- apply (Closure id env body) argument = eval body (Map.insert id argument env)
apply (Closure func) argument = func argument
apply _ _ = error "apply: invalid arguments"

-- eval :: Expr -> Env -> Value
-- eval expr env = case expr of
--     Constant integer -> Integer integer
--     Add left right -> add (eval left env) (eval right env)
--     Variable id -> fromJust (env !? id)
--     Lambda id body -> Closure (\value -> eval body (Map.insert id value env) P.id)
--     Application e1 e2 -> eval e1 env `apply` eval e2 env

evalK :: Expr -> Env -> (Value -> Value) -> Value
evalK expr env k = case expr of
    Constant integer -> k (Integer integer)
    Add left right -> evalK left env (\leftV -> evalK right env (\rightV -> add leftV rightV))
    Variable id -> k (fromJust (env !? id))
    Lambda id body -> k (Closure (\value k' -> evalK body (Map.insert id value env) k'))
    Application e1 e2 -> evalK e1 env (\func -> evalK e2 env (\arg -> apply func arg k))

-- data ExprK
--     = ConstantK Identifier Int
--     | Add Identifier Expr Expr
--     | Variable Identifier Identifier
--     | Lambda Identifier Identifier Expr
--     | Application Identifier Expr Expr

-- evalK' :: ExprK -> Env -> Value -> Value
-- evalK' expr env closure = case expr of
--     ConstantK k integer -> apply closure (Integer integer)
    -- Add id left right -> add (evalK' left env) (evalK' right env)
    -- Variable id1 id2 -> fromJust  (env  !? id1) `apply` evalK' id2 env

