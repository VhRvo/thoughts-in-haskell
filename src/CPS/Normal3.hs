{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module CPS.Normal3 where

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

apply :: Value -> Value -> Cont -> Value
apply (VLam id env body) argument cont =
  evalK body (Map.insert id argument env) cont
apply _ _ _ = error "apply: invalid arguments"

data DCont
  = Halt
  | EvalOperator
  | EvalOperand

evalK :: Exp -> Env -> Cont -> Value
evalK expr = case expr of
  EInt int ->
    \_ k -> k (VInt int)
  EVar id ->
    \env k -> k (fromJust (env !? id))
  ELam id body ->
    \env k -> k (VLam id env body)
  EApp e1 e2 ->
    \env k ->
       evalK e1 env 
        (\func -> evalK e2 env 
         (\arg -> apply func arg k))
  EIf e1 e2 e3 ->
    \env k -> evalK e1 env
     (\premise ->
      if testPremise premise
        then evalK e2 env k
        else evalK e3 env k)

testPremise :: Value -> Bool
testPremise = undefined

data Exp1
  = E1Int Int
  | E1Var Identifier
  | E1Lam Identifier Exp
  | E1App Exp Exp
  | E1If Exp Exp Exp

applyExp1 :: Exp1 -> Env -> Cont -> Value
applyExp1 dcont =
  case dcont of
    E1Int int ->
      \_ k -> k (VInt int)
    E1Var id ->
      \env k -> k (fromJust (env !? id))
    E1Lam id body ->
      \env k -> k (VLam id env body)
    E1App e1 e2 ->
      \env k ->
         evalK e1 env
          (\func -> evalK e2 env 
           (\arg -> apply func arg k))
    E1If e1 e2 e3 ->
      \env k -> evalK e1 env
       (\premise ->
        if testPremise premise
          then evalK e2 env k
          else evalK e3 env k)

evalK1 :: Exp -> Env -> Cont -> Value
evalK1 expr = case expr of
  EInt int ->
    \_ k -> k (VInt int)
  EVar id ->
    \env k -> k (fromJust (env !? id))
  ELam id body ->
    \env k -> k (VLam id env body)
  EApp e1 e2 ->
    \env k ->
      evalK1 e1 env
       (\func -> evalK1 e2 env 
        (\arg -> apply func arg k))
  EIf e1 e2 e3 ->
    \env k -> evalK e1 env
     (\premise ->
      if testPremise premise
        then evalK e2 env k
        else evalK e3 env k)