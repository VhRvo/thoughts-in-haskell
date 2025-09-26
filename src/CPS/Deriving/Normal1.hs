{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module CPS.Deriving.Normal1 where

import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Prelude hiding (id)

newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show)

type Cont = Value -> Value

data Exp
  = EInt Int
  | EVar Identifier
  | ELam Identifier Exp
  | EApp Exp Exp
  | EIf Exp Exp Exp

data Value
  = VInt Int
  | VLam Identifier Env Exp

type Env = Map Identifier Value

apply :: Value -> Value -> DCont -> Value
apply (VLam id env body) argument cont =
  evalK body (Map.insert id argument env) cont
apply _ _ _ = error "apply: invalid arguments"

data DCont
  = ContHalt
  | ContOperator Exp Env DCont
  | ContOperand Value DCont
  | ContBranch Env Exp Exp DCont

applyDCont :: DCont -> Value -> Value
applyDCont cont value =
  case cont of
    ContHalt -> value
    ContOperator e2 env k ->
      let func = value
       in evalK e2 env (ContOperand func k)
    ContOperand func k ->
      let arg = value
       in apply func arg k
    ContBranch env e2 e3 k ->
      let premise = value
       in if testPremise premise
            then evalK e2 env k
            else evalK e3 env k

evalK :: Exp -> Env -> DCont -> Value
evalK expr = case expr of
  EInt int ->
    \_ k -> applyDCont k (VInt int)
  EVar id ->
    \env k -> applyDCont k (fromJust (env !? id))
  ELam id body ->
    \env k -> applyDCont k (VLam id env body)
  EApp e1 e2 ->
    \env k ->
      evalK
        e1
        env
        (ContOperator e2 env k)
  EIf e1 e2 e3 ->
    \env k ->
      evalK
        e1
        env
        (ContBranch env e2 e3 k)

testPremise :: Value -> Bool
testPremise = undefined

data Exp1
  = E1Int Int
  | E1Var Identifier
  | E1Lam Identifier Exp
  | E1App Exp Exp
  | E1If Exp Exp Exp

-- applyExp1 :: Exp1 -> Env -> Cont -> Value
-- applyExp1 exp =
--   case exp of
--     E1Int int ->
--       \_ k -> k (VInt int)
--     E1Var id ->
--       \env k -> k (fromJust (env !? id))
--     E1Lam id body ->
--       \env k -> k (VLam id env body)
--     E1App e1 e2 ->
--       \env k ->
--         evalK
--           e1
--           env
--           ( \func ->
--               evalK
--                 e2
--                 env
--                 (\arg -> apply func arg k)
--           )
--     E1If e1 e2 e3 ->
--       \env k ->
--         evalK
--           e1
--           env
--           ( \premise ->
--               if testPremise premise
--                 then evalK e2 env k
--                 else evalK e3 env k
--           )

-- evalK1 :: Exp -> Env -> Cont -> Value
-- evalK1 expr = case expr of
--   EInt int ->
--     applyExp1 (E1Int int)
--   EVar id ->
--     applyExp1 (E1Var id)
--   ELam id body ->
--     applyExp1 (E1Lam id body)
--   EApp e1 e2 ->
--     applyExp1 (E1App e1 e2)
--   EIf e1 e2 e3 ->
--     applyExp1 (E1If e1 e2 e3)




