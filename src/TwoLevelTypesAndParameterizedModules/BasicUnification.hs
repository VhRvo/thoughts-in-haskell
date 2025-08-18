module TwoLevelTypesAndParameterizedModules.BasicUnification where

-- import qualified Data.Text as T
import Control.Monad.ST
import Data.STRef
import Data.Text (Text)

type Ptr a = STRef a (Maybe (TypeExpr a))

data TypeExpr a
  = MutVar (Ptr a)
  | GenVar Int
  | OperatorType Text [TypeExpr a]

prune ::
  forall a. TypeExpr a -> ST a (TypeExpr a)
prune expr =
  case expr of
    MutVar ptr ->
      readSTRef ptr >>= \case
        Nothing -> pure expr
        Just pointee -> do
          pointee <- prune pointee
          writeSTRef ptr (Just pointee)
          pure pointee
    _ -> return expr

occursInType ::
  forall a. Ptr a -> TypeExpr a -> ST a Bool
occursInType ptr expr = do
  expr' <- prune expr
  case expr' of
    MutVar ptr' -> pure (ptr == ptr')
    GenVar _ -> pure False
    OperatorType _ arguments -> do
      results <- traverse (occursInType ptr) arguments
      pure (or results)

unifyType ::
  forall a. TypeExpr a -> TypeExpr a -> ST a ()
unifyType lhs rhs = do
  lhs <- prune lhs
  rhs <- prune rhs
  case (lhs, rhs) of
    (MutVar ptr, MutVar ptr') ->
      if ptr == ptr'
        then pure ()
        else writeSTRef ptr (Just rhs)
    (MutVar ptr, _) ->
      singleBind ptr rhs
    (_, MutVar ptr) ->
      singleBind ptr lhs
    (GenVar index, GenVar index') ->
      if index == index'
        then pure ()
        else error "different generic variables"
    (OperatorType operator arguments, OperatorType operator' arguments') -> do
      if operator == operator'
        then unifyArguments arguments arguments'
        else error "different construtors"
    _ ->
      error "different types"
  where
    singleBind :: Ptr a -> TypeExpr a -> ST a ()
    singleBind ptr expr = do
      occured <- occursInType ptr expr
      if occured
        then error "occurs in"
        else writeSTRef ptr (Just expr)

    unifyArguments :: [TypeExpr a] -> [TypeExpr a] -> ST a ()
    unifyArguments [] [] = pure ()
    unifyArguments (x : xs) (y : ys) = do
      unifyType x y
      unifyArguments xs ys
    unifyArguments _ _ =
      error "different lengths"

instantiate ::
  forall a. [TypeExpr a] -> TypeExpr a -> TypeExpr a
instantiate types type' =
  case type' of
    MutVar _ ->
      type'
    GenVar index ->
      types !! index
    OperatorType operator arguments ->
      OperatorType operator (fmap (instantiate types) arguments)
