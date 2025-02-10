{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module GADTsInAction where

import Data.Data (Proxy (..))
import qualified Data.Kind as K
import Data.Type.Equality
import Data.Text (Text)
import Data.Function (on)

data Type :: K.Type -> K.Type where
  TInt :: Type Int
  TBool :: Type Bool
  TArrow :: Type a -> Type b -> Type (a -> b)

data Bop :: K.Type -> K.Type -> K.Type where
  Add :: Bop Int Int
  Sub :: Bop Int Int
  Eq :: Bop Int Bool
  Lt :: Bop Int Bool
  Gt :: Bop Int Bool
  And :: Bop Bool Bool
  Or :: Bop Bool Bool

class TypeOf a where
  typeOf :: Proxy a -> Type a

instance TypeOf Int where
  typeOf :: Proxy Int -> Type Int
  typeOf Proxy = TInt

instance TypeOf Bool where
  typeOf :: Proxy Bool -> Type Bool
  typeOf Proxy = TBool

class TypeOfExpr a where
  typeOfExpr :: Expr a -> Type a

instance TypeOfExpr Int where
  typeOfExpr :: Expr Int -> Type Int
  typeOfExpr _ = TInt

instance TypeOfExpr Bool where
  typeOfExpr :: Expr Bool -> Type Bool
  typeOfExpr _ = TBool

instance (TypeOfExpr b) => TypeOfExpr (a -> b) where
  typeOfExpr :: (TypeOfExpr b) => Expr (a -> b) -> Type (a -> b)
  typeOfExpr expr = case expr of
    Var _ t -> t
    Lambda _ t e -> TArrow t $ typeOfExpr e
    App func _ -> case typeOfExpr func of
      TArrow _ t2 -> t2
    If _ e1 _ -> typeOfExpr e1
    Lift _ t -> t
    _ -> error "Wrong expression type in typeOfExpr"

eqTy :: forall u v. Type u -> Type v -> Maybe (u :~: v)
eqTy TInt TInt = Just Refl
eqTy TBool TBool = Just Refl
eqTy (TArrow u1 u2) (TArrow v1 v2) = do
  Refl <- eqTy u1 v1
  Refl <- eqTy u2 v2
  pure Refl
eqTy _ _ = Nothing

data Expr :: K.Type -> K.Type where
  Lit :: (TypeOf a) => a -> Expr a
  Var :: Text -> Type a -> Expr a
  Lambda :: Text -> Type a -> Expr b -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b
  Bop :: Bop a b -> Expr a -> Expr a -> Expr b
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  Lift :: a -> Type a -> Expr a

plus :: Expr (Int -> Int -> Int)
plus = Lambda "x" TInt $ Lambda "y" TInt $ Bop Add (Var "x" TInt) (Var "y" TInt)

abs :: Expr (Int -> Int)
abs =
  Lambda "x" TInt $
    If
      (Bop Lt (Var "x" TInt) (Lit 0))
      {- then -} (Bop Sub (Lit 0) (Var "x" TInt))
      {- else -} (Var "x" TInt)

-- The first argument is the identifier of the variable we are substituting.
-- The second argument is the value that we are substituting and the third is its type.
-- The fourth argument is the expression we are substituting in.
-- The function then returns resulting expression after the substitution.
subst :: forall u t. Text -> u -> Type u -> Expr t -> Expr t
subst _ _ _ (Lit lit) = Lit lit
subst x v u (Var y t)
  | x == y = case eqTy u t of
      Just Refl -> Lift v u
      Nothing -> error "ill-typed substitution"
  | otherwise = Var y t
subst x v u (Bop b e1 e2) = Bop b (subst x v u e1) (subst x v u e2)
subst x v u (If c1 e1 e2) = If (subst x v u c1) (subst x v u e1) (subst x v u e2)
subst x v u (Lambda y t e)
  | x == y = Lambda y t e
  | otherwise = Lambda y t (subst x v u e)
subst x v u (App e1 e2) = App (subst x v u e1) (subst x v u e2)
subst _ _ _ (Lift x t) = Lift x t

eval :: Expr t -> t
eval expr = case expr of
  Lit lit -> lit
  Var name tpe -> error "Free variable has no value"
  Lambda x t body -> \value -> eval $ subst x value t body
  App e1 e2 -> eval e1 (eval e2)
  Bop b e1 e2 -> (evalBop b `on` eval) e1 e2
  If b e1 e2
    | eval b -> eval e1
    | otherwise -> eval e2
  Lift x _ -> x

evalBop :: Bop a b -> a -> a -> b
evalBop op = case op of
  Add -> (+)
  Sub -> (-)
  Eq -> (==)
  Lt -> (<)
  Gt -> (>)
  And -> (&&)
  Or -> (||)

