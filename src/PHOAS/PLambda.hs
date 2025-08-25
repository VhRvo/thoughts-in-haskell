{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module PHOAS.PLambda where

data BinaryOp
  = Add
  | Mult
  | Eq

data PLambda a
  = Var a
  | Int Int
  | Bool Bool
  | If (PLambda a) (PLambda a) (PLambda a)
  | Binary BinaryOp (PLambda a) (PLambda a)
  | Lam (a -> PLambda a)
  | App (PLambda a) (PLambda a)

newtype Lambda = Hide { reveal :: forall a. PLambda a }

data Value
  = VInt Int
  | VBool Bool
  | VFunction (Value -> Value)

instance Show Value where
  show :: Value -> String
  show (VInt int) = show int
  show (VBool bool) = show bool
  show (VFunction _) = "<Function>"

eval :: Lambda -> Value
eval expr = eval' (reveal expr)
  where
    eval' :: PLambda Value -> Value
    eval' expr = case expr of
      Var value -> value
      Int number -> VInt number
      Bool bool -> VBool bool
      If e1 e2 e3 ->
        case eval' e1 of
          VBool bool -> if bool then eval' e2 else eval' e3
          _ -> error ""
      Binary op e1 e2 -> evalBinary op (eval' e1) (eval' e2)
      Lam f -> VFunction (\x -> eval' (f x))
      App e1 e2 ->
        case eval' e1 of
          VFunction f -> f (eval' e2)
          _ -> error ""

    evalBinary :: BinaryOp -> Value -> Value -> Value
    evalBinary Add  (VInt lhs) (VInt rhs) = VInt (lhs + rhs)
    evalBinary Mult (VInt lhs) (VInt rhs) = VInt (lhs * rhs)
    evalBinary Eq (VInt lhs) (VInt rhs) = VBool (lhs == rhs)
    evalBinary _ _ _ = error ""

test1 :: Lambda
test1 = Hide (App (Lam (\x -> Binary Add (Int 3) (Var x))) (Int 4))

--- >>> show (eval test1)
-- "7"


