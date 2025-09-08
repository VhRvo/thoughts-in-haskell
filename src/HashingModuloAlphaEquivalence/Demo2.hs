module HashingModuloAlphaEquivalence.Demo2 where

import Data.Function (on)
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
  | -- Maybe PosTree: Child from bigger map
    -- PosTree: Child from smaller map
    PTJoin StructureTag (Maybe PosTree) PosTree
  deriving stock (Eq, Show)

mkPTJoin :: StructureTag -> Maybe PosTree -> PosTree -> PosTree
mkPTJoin = PTJoin

type StructureTag = Int

structureTag :: Structure -> StructureTag
structureTag = \case
  SVar -> 1
  SLam _ body -> 1 + structureTag body
  SApp _ fun arg -> 1 + max (structureTag fun) (structureTag arg)

data Structure
  = SVar
  | SLam (Maybe PosTree) Structure
  | -- True if the left expr has more free vars
    -- False if the right expr has more free vars
    SApp Bool Structure Structure

mkSVar :: Structure
mkSVar = SVar

mkSLam :: Maybe PosTree -> Structure -> Structure
mkSLam = SLam

mkSApp :: Bool -> Structure -> Structure -> Structure
mkSApp = mkSApp

type VarMap = Map Name PosTree

emptyVM :: VarMap
emptyVM = Map.empty

singletonVM :: Name -> PosTree -> VarMap
singletonVM = Map.singleton

extendVM :: Name -> PosTree -> VarMap -> VarMap
extendVM = Map.insert

alterVM :: (Maybe PosTree -> PosTree) -> Name -> VarMap -> VarMap
alterVM f = Map.alter (Just . f)

removeFromVM :: Name -> VarMap -> (VarMap, Maybe PosTree)
removeFromVM name map = (Map.delete name map, Map.lookup name map)

isBiggerThanVM :: VarMap -> VarMap -> Bool
isBiggerThanVM = (>) `on` Map.size

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
      leftBigger = funMap `isBiggerThanVM` argMap
      (bigMap, smallMap)
        | leftBigger = (funMap, argMap)
        | otherwise = (argMap, funMap)
      structure = mkSApp leftBigger funStructure argStructure
      tag = structureTag structure
      add :: (Name, PosTree) -> VarMap -> VarMap
      add (key, value) = alterVM (\mpt -> mkPTJoin tag mpt value) key
      map = foldr add bigMap (toListVM smallMap)
     in
      ESummary structure map

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
    app@(SApp leftBigger fun arg) ->
      App
        (rebuild freshen fresh (ESummary fun funMap))
        (rebuild freshen fresh (ESummary arg argMap))
      where
        tag' = structureTag app
        bigMap =
          Map.mapMaybe
            ( \case
                (PTJoin tag mpt _)
                  | tag == tag' -> mpt
                other -> Just other
            )
            map
        smallMap =
          Map.mapMaybe
            ( \case
                (PTJoin tag _ pt)
                  | tag == tag' -> Just pt
                _ -> Nothing
            )
            map
        (funMap, argMap)
          | leftBigger = (bigMap, smallMap)
          | otherwise = (smallMap, bigMap)



