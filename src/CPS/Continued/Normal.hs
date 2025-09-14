{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module CPS.Continued.Normal where

import Data.Text (Text)

data Exp
  = EVar Text
  | EInt Int
  | ELam Text Exp
  | EApp Exp Exp
--   | EIf Exp Exp Exp
  | ELet Text Exp Exp

-- data Value
--   = VInt Int
--   | VLam Text CExp

data CExp
  = LetVal Text CValue CExp
  | LetCont Text Text CExp CExp
  | CAppC Text Text
  | CApp Text Text Text
--   | KIf Text Text Text

data CValue
  = CVInt Int
  | CVLam Text Text CExp

cps :: Exp -> (Text -> CExp) -> CExp
cps exp k =
 case exp of
  EVar x -> k x
--   EInt int -> cps
  EApp fun arg ->
    cps fun (\fun' ->
        cps arg (\arg' ->
            LetCont "k" "x" (k "x")
            (CApp fun' "k" arg')))
  _ -> undefined

