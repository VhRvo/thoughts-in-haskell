module CodeGen where

import Data.Char (digitToInt, isDigit)

data Expression
  = Num Int
  | Add Expression Expression
  | Sub Expression Expression
  deriving (Show)

emit :: Expression -> String
emit = \case
  Num x -> show x
  Add left right -> emit left <> " + " <> emit right
  Sub left right -> emit left <> " - " <> emit right

expected :: String -> a
expected x = error $ x <> "expected"

term :: Char -> Expression
term x
  | isDigit x = Num (digitToInt x)
  | otherwise = expected "Digit"

addOperation :: Char -> Expression -> Expression -> Expression
addOperation x
  | x == '+' = Add
  | x == '-' = Sub
  | otherwise = expected "AddOp"

expression :: String -> Expression
expression = \case
  [x] -> term x
  (x : y : zs) -> addOperation y (expression [x]) (expression zs)
  _ -> expected "Input"

-- >>> expression "1+1-2"

emitLn :: String -> String
emitLn s = "    " <> s <> "\n"

add, sub, pushEax :: String
add = emitLn "ADD eax, ebx"
sub = emitLn "SUB eax, ebx" <> emitLn "NEG eax"
pushEax = emitLn "MOV ebx, eax"

-- composition
emitAsm :: Expression -> String
emitAsm = \case
  Num a -> emitLn ("MOV eax, " <> show a)
  Add left right -> emitAsm left <> pushEax <> emitAsm right <> add
  Sub left right -> emitAsm left <> pushEax <> emitAsm right <> sub

parseAndEmit :: String -> String
parseAndEmit = emitAsm . expression

-- docker run -it --rm -v /Users/zhangyongzhuo/Desktop/PekingKoopa:/root/compiler maxxing/compiler-dev bash
