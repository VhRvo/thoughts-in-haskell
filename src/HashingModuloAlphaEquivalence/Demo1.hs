module HashingModuloAlphaEquivalence.Demo1 where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Name = Text

data Exp
  = Var Name
  | Lam Name Exp
  | App Exp Exp

data ESummary
  = ESummary Structure VarMap

data Structure
  = SVar
  | SLam (Maybe PosTree) Structure
  | SApp Structure Structure

mkSVar :: Structure
mkSVar = SVar

mkSLam :: Maybe PosTree -> Structure -> Structure
mkSLam = SLam

mkSApp :: Structure -> Structure -> Structure
mkSApp = mkSApp

type VarMap = Map Name PosTree

emptyVM :: VarMap
emptyVM = Map.empty

singletonVM :: Name -> PosTree -> VarMap
singletonVM = Map.singleton

extendVM :: Name -> PosTree -> VarMap -> VarMap
extendVM = Map.insert

removeFromVM :: Name -> VarMap -> (VarMap, Maybe PosTree)
removeFromVM name map = (Map.delete name map, Map.lookup name map)

toListVM :: VarMap -> [(Name, PosTree)]
toListVM = Map.toList

data PosTree
  = PTHere
  | PTLeftOnly PosTree
  | PTRightOnly PosTree
  | PTBoth PosTree PosTree

summariseExp :: Exp -> ESummary
summariseExp = \case
  Var name ->
    ESummary mkSVar (singletonVM name PTHere)
  Lam name body ->
    let
      ESummary structure map = summariseExp body
    in
      undefined
  App fun arg ->
    undefined

rebuild :: ESummary -> Exp
rebuild = undefined

