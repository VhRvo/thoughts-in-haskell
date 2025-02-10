module RegExpr.List where

import Control.Monad.Cont
import Control.Monad.State
import RegExpr.Expr

accept :: Expr -> String -> Cont Bool String
accept expr input = case expr of
  Zero -> cont . const $ False
  One -> cont $ \k -> k input
  Literal ch -> case input of
    "" -> cont . const $ False
    ch' : rest
      | ch == ch' -> pure rest
      | otherwise -> cont . const $ False
  Or left right -> cont $ \k ->
    runCont (accept left input) k || runCont (accept right input) k
  And left right -> do
    rest <- accept left input
    accept right rest
  Many expr' -> do
    acceptMany expr' input

acceptMany :: Expr -> String -> Cont Bool String
acceptMany expr input = cont $ \k ->
  k input || runCont (accept expr input) (\rest -> rest /= input && runCont (acceptMany expr rest) k)

--   One
--   Literal Char
--   Or Expr Expr
--   And Expr Expr
--   Many Expr
--   Many Expr

-- match' :: Expr -> StateT String (Cont Bool) ()
-- match' = \case
--   Zero -> StateT $ \_ -> cont . const $ False
--   One -> pure ()
--   Literal -> do
--     input <- get
--     case input of
--         "" -> 

--   One -> cont . const . null $ input
--   Literal ch -> case input of
--     "" -> cont . const $ False
--     ch' : rest
--       | ch == ch' -> pure rest
--       | otherwise -> cont . const $ False
--   Or left right -> cont $ \k ->
--     runCont (match left input) k || runCont (match right input) k
--   And left right -> do
--     rest <- match left input
--     match right rest
