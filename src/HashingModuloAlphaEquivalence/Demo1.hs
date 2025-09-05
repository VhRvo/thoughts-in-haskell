module HashingModuloAlphaEquivalence.Demo1 where

import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

type Name = Text

data Exp
  = Var Name
  | Lam Name Exp
  | App Exp Exp

data ESummary
  = ESummary Structure VarMap

data PosTree
  = PTHere
  | PTLeftOnly PosTree
  | PTRightOnly PosTree
  | PTBoth PosTree PosTree
  deriving stock (Eq, Show)

pickL :: PosTree -> Maybe PosTree
pickL (PTLeftOnly left) = Just left
pickL (PTBoth left _)   = Just left
pickL _                 = Nothing

pickR :: PosTree -> Maybe PosTree
pickR (PTRightOnly right) = Just right
pickR (PTBoth _ right)    = Just right
pickR _                   = Nothing

renderPosTree :: PosTree -> Text
renderPosTree = go
  where
    go PTHere              = "H"
    go (PTLeftOnly left)   = "L" <> go left
    go (PTRightOnly right) = "R" <> go right
    go (PTBoth left right) = "B[" <> go left <> "][" <> go right <> "]"

-- >>> renderPosTree PTHere
-- >>> renderPosTree (PTBoth (PTBoth (PTLeftOnly (PTRightOnly PTHere)) (PTRightOnly (PTLeftOnly (PTRightOnly PTHere)))) (PTLeftOnly (PTRightOnly PTHere)))
-- "H"
-- "B[B[LRH][RLRH]][LRH]"


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

mergeVM ::
  (PosTree -> PosTree) ->
  (PosTree -> PosTree) ->
  (PosTree -> PosTree -> PosTree) ->
  VarMap ->
  VarMap ->
  VarMap
mergeVM left right both =
  Map.merge
    (Map.mapMissing (const left))
    (Map.mapMissing (const right))
    (Map.zipWithMatched (const both))

findSingletonVM :: VarMap -> Name
findSingletonVM map =
  case toListVM map of
    [(key, PTHere)] -> key
    _ -> error "findSingletonVM: there are too many variables in map"

--- >>> mergeVM PTLeftOnly PTRightOnly PTBoth (singletonVM "left" PTHere) emptyVM
-- fromList [("left",PTLeftOnly PTHere)]
--- >>> mergeVM PTLeftOnly PTRightOnly PTBoth emptyVM (singletonVM "right" PTHere)
-- fromList [("right",PTRightOnly PTHere)]
--- >>> mergeVM PTLeftOnly PTRightOnly PTBoth (singletonVM "both" (PTLeftOnly PTHere)) (singletonVM "both" PTHere)
-- fromList [("both",PTBoth (PTLeftOnly PTHere) PTHere)]

summariseExp :: Exp -> ESummary
summariseExp = \case
  Var name ->
    ESummary mkSVar (singletonVM name PTHere)
  Lam name body ->
    let
      ESummary bodyStructure bodyMap = summariseExp body
      (map, posTree) = removeFromVM name bodyMap
     in
      ESummary (mkSLam posTree bodyStructure) map
  App fun arg ->
    let
      ESummary funStructure funMap = summariseExp fun
      ESummary argStructure argMap = summariseExp arg
      merge = mergeVM PTLeftOnly PTRightOnly PTBoth
     in
      ESummary (mkSApp funStructure argStructure) (merge funMap argMap)

rebuild :: (Name -> Name) -> Name -> ESummary -> Exp
rebuild freshen fresh (ESummary structure map) =
  case structure of
    SVar -> Var (findSingletonVM map)
    SLam posTree body ->
      Lam fresh (rebuild freshen (freshen fresh) (ESummary body map'))
      where
        map' =
          case posTree of
            Nothing -> map
            Just posTree' -> extendVM fresh posTree' map
    SApp fun arg ->
      App
        (rebuild freshen fresh (ESummary fun (Map.mapMaybe pickL map)))
        (rebuild freshen fresh (ESummary arg (Map.mapMaybe pickR map)))


