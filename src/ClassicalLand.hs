module ClassicalLand where

import Data.Text (Text)
import Data.Text qualified as T
import Prelude hiding (False, not, (^))
import Prelude qualified as P

data Proposition
  = Proposition :-> Proposition
  | Symbol Text
  | False
  deriving (Eq)

not :: Proposition -> Proposition
not p = p :-> False

instance Show Proposition where
  show :: Proposition -> String
  show = \case
    a :-> b -> "(" <> show a <> " -> " <> show b <> ")"
    Symbol s -> T.unpack s
    False -> "False"

data Proof
  = ModusPones Proof Proof
  | Axiom Text Proposition
  deriving (Eq)

instance Show Proof where
  show :: Proof -> String
  show = \case
    Axiom n t -> "(" <> T.unpack n <> " :: " <> show t <> ")"
    ModusPones f x -> "(" <> show f <> " " <> show x <> ")"

source :: Proposition -> Proposition
source (a :-> _) = a

target :: Proposition -> Proposition
target (_ :-> b) = b

consequence :: Proof -> Proposition
consequence = \case
  Axiom _ p -> p
  -- Provided f stands for axiom.
  -- What if f stands for another modus pones?
  ModusPones f g -> target (consequence f)

infixl 5 @@

(@@) :: Proof -> Proof -> Proof
f @@ g
  | source (consequence f) == consequence g = ModusPones f g
  | otherwise = error ("@@ error: " <> show f <> " " <> show g)

a :: Proposition -> Proof
a m@(p :-> (q :-> p'))
  | p == p' = Axiom "a" m

b :: Proposition -> Proof
b m@((p :-> (q :-> r)) :-> ((p' :-> q') :-> (p'' :-> r')))
  | p == p' && p == p'' && q == q' && r == r' = Axiom "b" m

c :: Proposition -> Proof
c m@(((p :-> False) :-> False) :-> p')
  | p == p' = Axiom "c" m

identity :: Proposition -> Proof
identity p = b1 @@ a1 @@ a2
  where
    b1 =
      b
        ( (p :-> (p :-> p) :-> p)
            :-> (p :-> p :-> p)
            :-> (p :-> p)
        )
    a1 = a (p :-> (p :-> p) :-> p)
    a2 = a (p :-> p :-> p)

class Translatable a where
  (^) :: a -> Proposition -> a

instance Translatable Proposition where
  False ^ k = k
  (p :-> q) ^ k = (p ^ k) :-> (q ^ k)
  p ^ k = (p :-> k) :-> k

-- consequence (x ^ k) == consequence x ^ k
free :: Text -> Proof -> Bool
free n (Axiom n' p) = n /= n'
free n (ModusPones a b) = free n a && free n b

elim :: Proof -> Proof -> Proof
elim (Axiom n p) (Axiom n' q) =
  if n == n' -- not p == q?
    then identity p
    else a (q :-> (p :-> q)) @@ Axiom n' q
elim (Axiom n p) m@(ModusPones q r) =
  if free n m
    then
      -- optimization
      let s = consequence m
       in a (s :-> p :-> s) @@ m
    else
      let
        q' = elim (Axiom n p) q
        r' = elim (Axiom n p) r
        tj = consequence r
        ti = target (consequence q)
       in
        b
          ( (p :-> (tj :-> ti))
              :-> ((p :-> tj) :-> (p :-> ti))
          )
          @@ q'
          @@ r'

magic :: Proposition -> Proof
magic (False :-> q) = elim u1 $ c1 @@ (a1 @@ u1)
  where
    a1 = a (False :-> ((q :-> False) :-> False))
    c1 = c (not (not q) :-> q)
    u1 = Axiom "u" False
