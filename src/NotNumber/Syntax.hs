module NotNumber.Syntax where

import Control.Applicative (Alternative (..))
import Data.Text (Text)

-- type Name = Text
type Name = Stack (Text, Int)

infixl 9 :$

infixr 6 :->

data Exp
  = F Name
  | B Int
  | Exp :$ Exp
  | Exp :-> Scope
  deriving (Show, Eq)

newtype Scope
  = Scope Exp
  deriving (Show, Eq)

substitute :: Exp -> Name -> Exp -> Exp
substitute image name = instantiate image . abstract name

abstract :: Name -> Exp -> Scope
abstract name = Scope . nameTo 0
  where
    nameTo :: Int -> Exp -> Exp
    nameTo outer = \case
      F name'
        | name == name' -> B outer
        | otherwise -> F name'
      B index -> B index
      fun :$ arg -> nameTo outer fun :$ nameTo outer arg
      domain :-> Scope body ->
        nameTo outer domain :-> Scope (nameTo (1 + outer) body)

-- >>> subst 0 image lam
-- F "b" :-> Scope (F "c" :-> Scope (F "a" :-> Scope (B 3 :$ B 0)))
-- >>> instantiate image lam
-- F "b" :-> Scope (F "c" :-> Scope (F "a" :-> Scope (B 3 :$ B 0)))
-- image :: Exp
-- image = F "a" :-> Scope (B 1 :$ B 0)

-- lam :: Scope
-- lam = Scope (F "b" :-> Scope (F "c" :-> Scope (B 2)))
-- lam = Scope lamBody

instantiate :: Exp -> Scope -> Exp
instantiate image (Scope body) = replace 0 body
  where
    replace :: Int -> Exp -> Exp
    replace outer = \case
      F name -> F name
      B index
        | index == outer -> shift outer 0 image
        | otherwise -> B index
      fun :$ arg -> replace outer fun :$ replace outer arg
      domain :-> Scope body ->
        replace outer domain :-> Scope (replace (1 + outer) body)

shift :: Int -> Int -> Exp -> Exp
shift outer inner = \case
  F name -> F name
  B index
    -- too tricky to use `<` instead of `<=`
    | index < inner ->
        B index
    | otherwise ->
        B (outer + index)
  fun :$ arg ->
    shift outer inner fun :$ shift outer inner arg
  domain :-> Scope body ->
    shift outer inner domain :-> Scope (shift outer (1 + inner) body)

subst :: Int -> Exp -> Scope -> Exp
subst target image (Scope body) = replace body
  where
    replace :: Exp -> Exp
    replace = \case
      F name -> F name
      B index
        | index == target -> image
        | otherwise -> B index
      fun :$ arg -> replace fun :$ replace arg
      domain :-> body ->
        -- inefficient substitution
        replace domain :-> Scope (subst (1 + target) (shift 1 0 image) body)

unapply :: (Alternative m) => Exp -> m (Exp, Exp)
unapply (fun :$ arg) = pure (fun, arg)
unapply _ = empty

infix 5 :<-

data Binding = Name :<- Exp

bName :: Binding -> Name
bName (name :<- _) = name

bVar :: Binding -> Exp
bVar = F . bName

infixl 4 :<

data Stack x
  = Empty
  | Stack x :< x
  deriving (Show, Eq)

type Zipper = Stack Step

data Step
  = Fun () Exp
  | Arg Exp ()
  | Dom () Scope
  | Range Binding ()

infixl 4 <+

(<+) :: Stack a -> Stack a -> Stack a
xs <+ Empty = xs
xs <+ (ys :< y) = xs <+ ys :< y

infixl 6 //

(//) :: Name -> Text -> Name
root // string = root :< (string, 0)

name :: Text -> Name
name = (Empty //)

type Agency agent = Name -> agent

infixr 6 -->

(-->) :: Binding -> Exp -> Exp
(name :<- domain) --> range = domain :-> abstract name range

infix 9 <--

(<--) ::
  (Alternative m) =>
  Agency (Exp -> m (Binding, Exp))
name <-- (domain :-> scope) =
  pure (name :<- domain, instantiate (F name) scope)
_ <-- _ = empty

type Prefix = Stack Binding

infixr 6 ->>

(->>) :: Prefix -> Exp -> Exp
Empty ->> expr = expr
(bindings :< binding) ->> range = bindings ->> (binding --> range)

-- If root is independent of all the names in expr—which it will be,
-- if we maintain our hierarchical discipline—and
-- `unpreﬁx root x expr = (binds, range)`
-- then range is unquantiﬁed and expr = binds ->> range.

unprefix :: Agency (Text -> Exp -> (Prefix, Exp))
unprefix root id exp =
  intro 1 (Empty, exp)
  where
    intro :: Int -> (Prefix, Exp) -> (Prefix, Exp)
    intro _i (bindings, exp) =
      case (root :< (id, _i)) <-- exp of
        Just (binding, range) -> intro (1 + _i) (bindings :< binding, range)
        Nothing -> (bindings, exp)

weaken :: Agency (Exp -> Exp -> Exp)
weaken root domain exp =
  (xdomain :< ((root // "y") :<- domain)) ->> range
  where
    -- xdomain ->> (((root // "y") :<- domain) --> range)

    (xdomain, range) = unprefix root "x" exp

infixl 9 $$

($$) :: Exp -> [Exp] -> Exp
exp $$ [] = exp
fun $$ (arg : args) = (fun :$ arg) $$ args

unapplies :: Exp -> (Exp, [Exp])
unapplies exp = peel exp []
  where
    peel :: Exp -> [Exp] -> (Exp, [Exp])
    peel (fun :$ arg) args = peel fun (arg : args)
    peel nonApp args = (nonApp, args)

data Analysis = ForAll Prefix Name [Exp]

analysis :: Agency (Text -> Exp -> Analysis)
analysis root id expr =
  ForAll prefix f args
  where
    (prefix, range) = unprefix root id expr
    (F f, args) = unapplies range

infixl 9 -$$

(-$$) :: Name -> Prefix -> Exp
(-$$) f = apply (F f)
  where
    apply :: Exp -> Prefix -> Exp
    apply exp Empty = exp
    apply fun (bindings :< arg :<- _) = apply fun bindings :$ F arg

generalize :: Prefix -> Binding -> (Binding, Exp -> Exp)
generalize bindings (name :<- exp) =
  (name :<- bindings ->> exp, substitute (name -$$ bindings) name)
