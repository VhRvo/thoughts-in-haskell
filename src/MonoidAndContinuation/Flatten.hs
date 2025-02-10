module MonoidAndContinuation.Flatten where

import Test.QuickCheck
import GHC.Generics
import Generic.Random
import Text.Pretty.Simple

data Tree a
  = Tip a
  | Binary (Tree a) (Tree a)
  deriving (Eq, Show, Generic)

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = genericArbitrary (7 % 6 % ())
--   deriving Arbitrary via GenericArbitrary '[6, 7] (Tree a)

flatten :: Tree a -> [a]
flatten = \case
    Tip x -> [x]
    Binary left right -> flatten left <> flatten right

flatten1 :: Tree a -> ([a] -> [a])
flatten1 = \case
    Tip x -> ([x] <>)
    Binary left right -> flatten1 left . flatten1 right

extensionalEqualityProp1 :: (Eq a) => Tree a -> Bool
extensionalEqualityProp1 tree = flatten tree == flatten1 tree []

extensionalEqualityProp2 :: (Eq a) => Tree a -> Bool
extensionalEqualityProp2 tree = flatten tree == flatten2 tree [] []

extensionalEqualityProp3 :: (Eq a) => Tree a -> Bool
extensionalEqualityProp3 tree = flatten tree == flatten3 tree [] []

flatten2 :: Tree a -> [a] -> [Tree a] -> [a]
flatten2 tree xs k = case tree of
    Tip x -> abs k (x : xs)
    Binary t u -> flatten2 u xs (t : k)
  where
    abs [] xs = xs
    abs (t:k) xs = flatten2 t xs k

flatten3 :: Tree a -> [a] -> [Tree a] -> [a]
flatten3 tree xs k = case (tree, k) of
    (Tip x, []) -> x : xs
    (Tip x, t:k') -> flatten3 t (x: xs) k'
    (Binary t u, _) -> flatten3 u xs (t : k)


demo :: IO ()
demo = do
    -- generate (arbitrary @(Tree Int)) >>= pPrint
    quickCheck (extensionalEqualityProp1 @Int)
    quickCheck (extensionalEqualityProp2 @Int)
    quickCheck (extensionalEqualityProp3 @Int)
{-
ret n                             :: (Int -> Int) -> Int
sub                               :: (Int -> Int) -> Int -> Int -> Int
eval' e'                          :: (Int -> Int) -> Int
b2 (eval' e') sub                 :: (Int -> Int) -> Int -> Int
b1 (eval' e') (b2 (eval' e') sub) :: (Int -> Int) -> Int

ret n                             :: (Int -> Int) -> Arrow [] Int
sub                               :: (Int -> Int) -> Arrow [Int, Int] -> Int
eval' e'                          :: (Int -> Int) -> Arrow [] Int
b2 (eval' e') sub                 :: (Int -> Int) -> Arrow [Int] Int
b1 (eval' e') (b2 (eval' e') sub) :: (Int -> Int) -> Arrow [] Int

ret n ::                                       (Int -> Int) -> Int
halt ::                                                 Int -> Int
sub ::                           (Int -> Int) -> Int -> Int -> Int
b0 sub halt ::                                 Int -> (Int -> Int)
b1 (ret 5) (b0 sub halt) ::                             Int -> Int
b0 sub (b1 (ret 5) (b0 sub halt)) ::           Int -> (Int -> Int)
b1 (ret 4) (b0 sub (b1 (ret 5) (b0 sub halt))) ::       Int -> Int
b0 (ret 3) (b1 (ret 4) (b0 sub (b1 (ret 5) (b0 sub halt)))) :: Int

sub                               :: (Int -> Int) -> Arrow [Int, Int] -> Int
eval' e'                          :: (Int -> Int) -> Arrow [] Int
b2 (eval' e') sub                 :: (Int -> Int) -> Arrow [Int] Int
b1 (eval' e') (b2 (eval' e') sub) :: (Int -> Int) -> Arrow [] Int

eval5             :: Expr -> Int
eval5'            :: Expr -> (Int -> Int) -> Int
eval5' e          :: (Int -> Int) -> Int
halt              :: Int -> Int
b0 (eval5 e) halt :: Int

  eval5 `4 - (3 - (2 - (1 - 1)))`
= eval5' `4 - (3 - (2 - (1 - 1)))` halt
= eval5' (Diff `4` `3 - (2 - (1 - 1))`) halt
= eval5' (Diff `4` `3 - (2 - (1 - 1))`) halt
= b1 (eval5' `4`) (b2 (eval5' `3 - (2 - (1 - 1))`) sub)

  eval5 `1 - 1`
= b1 (eval5' `1`) (b2 (eval5' `1`) sub)
= b1 (ret 1) (b2 (ret 1) sub)

  eval5' `2 - (1 - 1)`
= b1 (eval5' `2`) (b2 (eval5' `1 - 1`) sub)
= b1 (ret 2) (b2 (b1 (ret 1) (b2 (ret 1) sub)) sub)

  eval5' `3 - (2 - (1 - 1))`
= b1 (ret 3) (b2 (eval5' `2 - (1 - 1)`) sub)
= b1 (ret 3) (b2 (b1 (ret 2) (b2 (b1 (ret 1) (b2 (ret 1) sub)) sub)) sub)

  eval5' `4 - (3 - (2 - (1 - 1)))` halt
= b1 (ret 4) (b2 (eval5' `3 - (2 - (1 - 1))`) sub)
= b1 (ret 4) (b2 (b1 (ret 3) (b2 (b1 (ret 2) (b2 (b1 (ret 1) (b2 (ret 1) sub)) sub)) sub)) sub)

  eval5' `4 - (3 - (2 - (1 - 1)))` halt
= b0 (eval5' `4 - (3 - (2 - (1 - 1)))`) halt
= b0 (b1 (ret 4) (b2 (eval5' `3 - (2 - (1 - 1))`) sub)) halt
= b0 (ret 4) (b0 (b2 (eval5' `3 - (2 - (1 - 1))`) sub) halt)
            = b1 (eval5' `3 - (2 - (1 - 1))`) (b0 sub halt)
            = b1 (b1 (ret 3) (b2 (eval5' `2 - (1 - 1)`) sub)) (b0 sub halt)
            = b1 (ret 3)
                 (b1 (b2 (eval5' `2 - (1 - 1)`) sub) (b0 sub halt))
                = b2 (eval5' `2 - (1 - 1)`) (b1 sub (b0 sub halt))
                = b2 (b1 (ret 2) (b2 (eval5' `1 - 1`) sub)) (b1 sub (b0 sub halt))
                = b2 (ret 2)
                     (b2 (b2 (eval5' `1 - 1`) sub) (b1 sub (b0 sub halt))))
                    = b3 (eval5' `1 - 1`) (b2 sub (b1 sub (b0 sub halt)))
                    = b3 (b1 (ret 1) (b2 (ret 1) sub)) (b2 sub (b1 sub (b0 sub halt)))
                    = b3 (ret 1)
                         (b3 (b2 (ret 1) sub) (b2 sub (b1 sub (b0 sub halt))))
                        = b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt))))
                    = b3 (ret 1) (b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt)))))
                = b2 (ret 2) (b3 (ret 1) (b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt))))))
            = b1 (ret 3) (b2 (ret 2) (b3 (ret 1) (b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt)))))))
= b0 (ret 4) (b1 (ret 3) (b2 (ret 2) (b3 (ret 1) (b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt))))))))

ret n : (Int -> Int) -> Int
        ~ Arrow [Int] Int -> Arrow [] Int
sub : (Int -> Int) -> (Int -> Int -> Int)
    ~ Arrow [Int] Int -> Arrow [Int, Int] Int
halt : Int -> Int
     ~ Arrow [] (Arrow [Int] Int)
b0 sub halt
  : Int -> Int -> Int
  ~ Arrow [Int] (Arrow [Int] Int)
b1 sub (b0 sub halt)
  : Int -> Int -> Int -> Int
  ~ Arrow [Int] (Arrow [Int, Int] Int)
  ~ Arrow [Int, Int] (Arrow [Int] Int)
b2 sub (b1 sub (b0 sub halt))
  : Int -> Int -> Int -> Int -> Int
  ~ Arrow [Int, Int] (Arrow [Int, Int] Int)
  ~ Arrow [Int, Int, Int] (Arrow [Int] Int)
b3 sub (b2 sub (b1 sub (b0 sub halt)))
  : Int -> Int -> Int -> Int -> Int -> Int
  ~ Arrow [Int, Int, Int] (Arrow [Int, Int] Int)
  ~ Arrow [Int, Int, Int, Int] (Arrow [Int] Int)
b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt))))
  : Int -> Int -> Int -> Int -> Int
  ~ Arrow [Int, Int, Int, Int] (Arrow [] Int)
  ~ Arrow [Int, Int, Int] (Arrow [Int] Int)
b3 (ret 1) (b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt)))))
  : Int -> Int -> Int -> Int
  ~ Arrow [Int, Int, Int] (Arrow [] Int)
  ~ Arrow [Int, Int] (Arrow [Int] Int)
b2 (ret 2) (b3 (ret 1) (b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt))))))
  : Int -> Int -> Int
  ~ Arrow [Int, Int] (Arrow [] Int)
  ~ Arrow [Int] (Arrow [Int] Int)
b1 (ret 3) (b2 (ret 2) (b3 (ret 1) (b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt)))))))
  : Int -> Int
  ~ Arrow [Int] (Arrow [] Int)
  ~ Arrow [] (Arrow [Int] Int)
b0 (ret 4) (b1 (ret 3) (b2 (ret 2) (b3 (ret 1) (b4 (ret 1) (b3 sub (b2 sub (b1 sub (b0 sub halt))))))))
  : Int
  ~ Arrow [] (Arrow [] Int)


  compile `4 - (3 - (2 - (1 - 1)))`
= [PushI 4] ++ compile `3 - (2 - (1 - 1))` ++ [SubI]
             = [PushI 3] ++ compile `2 - (1 - 1)` ++ [SubI]
                          = [PushI 2] ++ compile `1 - 1` ++ [SubI]
                                       = [PushI 1] ++ compile `1` ++ [SubI]
                                                    = [PushI 1]
                                       = [PushI 1, PushI 1, SubI]
                          = [PushI 2, PushI 1, PushI 1, SubI, SubI]
             = [PushI 3, PushI 2, PushI 1, PushI 1, SubI, SubI, SubI]
= [PushI 4, PushI 3, PushI 2, PushI 1, PushI 1, SubI, SubI, SubI, SubI]

  eval5' `(((4 - 3) - 2) - 1) - 1` halt
= b0 (eval5' `(((4 - 3) - 2) - 1) - 1`) halt
= b0 (b1 (eval5' `((4 - 3) - 2) - 1`) (b2 (eval5' `1`) sub)) halt
= b0 (b1 (eval5' `((4 - 3) - 2) - 1`) (b2 (ret 5) sub)) halt
= b0 (eval5' `((4 - 3) - 2) - 1`) (b0 (b2 (ret 5) sub) halt)
                                 = b1 (ret 5) (b0 sub halt)
    = b1 (eval5' `(4 - 3) - 2`) (b2 (eval5' `1`) sub)
    = b1 (eval5' `(4 - 3) - 2`) (b2 (ret 1) sub)
= b0 (b1 (eval5' `(4 - 3) - 2`) (b2 (ret 1) sub)) (b1 (ret 5) (b0 sub halt))
= b0 (eval5' `(4 - 3) - 2`) (b0 (b2 (ret 1) sub) (b1 (ret 5) (b0 sub halt)))
                           = b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt)))
    = b1 (eval5' `4 - 3`) (b2 (eval5' `2`) sub)
    = b1 (eval5' `4 - 3`) (b2 (ret 2) sub)
= b0 (b1 (eval5' `4 - 3`) (b2 (ret 2) sub)) (b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt))))
= b0 (eval5' `4 - 3`) (b0 (b2 (ret 2) sub) (b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt)))))
                     = b1 (ret 2) (b0 sub (b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt)))))
    = b1 (eval5' `4`) (b2 (eval5' `3`) sub)
    = b1 (eval5' `4`) (b2 (ret 3) sub)
= b0 (b1 (eval5' `4`) (b2 (ret 3) sub)) (b1 (ret 2) (b0 sub (b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt))))))
= b0 (eval5' `4`) (b0 (b2 (ret 3) sub) (b1 (ret 2) (b0 sub (b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt)))))))
                 = b0 (b2 (ret 3) sub) (b1 (ret 2) (b0 sub (b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt))))))
                 = b1 (ret 3) (b0 sub (b1 (ret 2) (b0 sub (b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt)))))))
= b0 (eval5' `4`) (b1 (ret 3) (b0 sub (b1 (ret 2) (b0 sub (b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt))))))))
    = ret 4
= b0 (ret 4) (b1 (ret 3) (b0 sub (b1 (ret 2) (b0 sub (b1 (ret 1) (b0 sub (b1 (ret 5) (b0 sub halt))))))))

x :: ExprRep7 bs
y :: ExprRep7 (Int : cs)
abs7 x :: Arrow bs Int
abs7 y :: Arrow (Int : cs) Int
        ~ Int -> Arrow cs r
b :: (b -> c) -> Arrow bs b -> Arrow as c
b :: (b -> Arrow cs r) -> Arrow bs b -> Arrow as (Arrow cs r)
b (abs7 y) (abs7 x) :: Arrow as (Arrow cs r)
                     ~ Arrow (as ++ cs) r
append7 x y :: ExprRep7 (as ++ bs)
abs7 (append7 x y) :: Arrow (as ++ bs) Int
b (abs7 y) (abs7 x) = abs7 (append7 x y)

Arrow as (Arrow bs Int) = Arrow (as ++ bs) Int)

case as of
  [] -> LHS
      = Arrow [] (Arrow bs Int)
      = Arrow bs Int
      = Arrow ([] ++ bs) Int
      = RHS
  t:ts -> LHS
      = Arrow (t:ts) (Arrow bs Int)
      = t -> Arrow ts (Arrow bs Int)
      = t -> Arrow (ts ++ bs) Int
      = Arrow (t : (ts ++ bs)) Int
      = Arrow ((t : ts) ++ bs) Int
      = RHS

b :: {as : List Type} -> Arrow [b] c -> Arrow as b -> Arrow as c
let c ~ Arrow ts r
b :: (b -> c) -> Arrow as b -> Arrow as c
   ~ (b -> (Arrow ts r)) -> Arrow as b -> Arrow as (Arrow ts r)
   ~ (b -> (Arrow ts r)) -> Arrow as b -> Arrow (as ++ ts) r

Arrow as (Arrow bs Int) = Arrow (as ++ bs) Int
case as of
  [] ->
    LHS = Arrow [] (Arrow bs Int)
        = Arrow bs Int
        = Arrow ([] ++ bs) Int = RHS
  t:ts ->
    LHS = Arrow (t:ts) (Arrow bs Int)
        = t -> Arrow ts (Arrow bs Int)
        = t -> Arrow (ts ++ bs) Int
        = Arrow (t : (ts ++ bs)) Int
        = Arrow ((t : ts) ++ bs) Int = RHS

  b^r (b^s+1 h g) f
= b^r+s h (b^r g f)

f :: Arrow ar c1
g :: Arrow (c1 : bs) c2
   ~ c1 -> Arrow bs c2
h :: c2 -> c3
b^r :: (b -> c) -> Arrow as b -> Arrow as c

b^s+1 h g :: Arrow (c1 : bs) c3
           ~ c1 -> Arrow bs c3
b^r (b^s+1 h g) f :: Arrow ar (Arrow bs c3)
                   ~ Arrow (ar ++ bs) c3

b^r g f :: Arrow ar (Arrow bs c2)
         ~ Arrow (ar ++ bs) c2
b^r+s h (b^r g f) :: Arrow (ar ++ bs) c3

  Arrow (ar ++ bs) c3
~ Arrow ar (Arrow bs c3)
~ b^r (_ : c2 -> Arrow bs c3) (_ : Arrow ar c2)
                                 ~ b^r (_ : c1 -> c2) (_ : Arrow ar c1)
               ~ b^s (_ : c2 -> c3) (_ : Arrow bs c2)


Arrow (as ++ [a]) b = Arrow as (a -> b)
  Arrow (as ++ [a]) b
= Arrow as (Arrow [a] b) -- lemma
= Arrow as (a -> b)

halt :: Int -> Int
      ~ Arrow [] (Arrow [Int] Int)
sub :: (Int -> r) -> Int -> Int -> r
     ~ Arrow [Int] r -> Arrow [Int, Int] r
b0 sub halt :: Int -> Int -> Int
             ~ Arrow [] (Arrow [Int, Int] Int)
             ~ Arrow [Int] (Arrow [Int] Int)
b1 sub (b0 sub halt) :: Int -> (Int -> Int -> r)
                      ~ Arrow [Int] (Arrow [Int, Int] Int)
                      ~ Arrow [Int, Int] (Arrow [Int] Int)
                      ~ Int -> Int -> (Int -> r)
b2 (b1 sub (b0 sub halt)) :: Int -> Int -> (Int -> Int -> r)
                           ~ Arrow [Int, Int] (Arrow [Int, Int] Int)
                           ~ Arrow [Int, Int, Int] (Arrow [Int] Int)
                           ~ Int -> Int -> Int -> (Int -> r)

as == [Int]
bs == [Int, Int]
cs == []

  b0 sub
= flip @(rev bs ++ rev cs) . flip @bs . f . flip @(rev as) . flip @(cs ++ as)
= flip2 . flip2 . f . flip1 . flip1

  b0 sub halt
= flip2 . flip2 . f . flip1 . flip1 $ halt

b0 sub halt :: Arrow [Int] (Arrow [Int] Int)
as == [Int]
bs == [Int, Int]
cs == [Int]

  b1 sub
= flip @(rev bs ++ rev cs) . flip @bs . f . flip @(rev as) . flip @(cs ++ as)
= flip3 . flip2 . f . flip1 . flip2

  b1 sub (b0 sub halt)
= flip3 . flip2 . f . flip1 . flip2 $ flip2 . flip2 . f . flip1 . flip1 $ halt
= flip3 . flip2 . f . flip1 . flip2 . flip2 . flip2 . f . flip1 . flip1 $ halt
= flip3 . flip2 . f . flip1 .       id      . flip2 . f . flip1 . flip1 $ halt
= flip3 . flip2 . f .   id  .                 flip2 . f .   id  . flip1 $ halt
= flip3 . flip2 . f .                         flip2 . f .           id  $ halt
= flip3 . flip2 . f .                         flip2 . f .               $ halt
= flip3 . flip2 . f .                         flip2 . f .               $ id halt

b1 sub (b0 sub halt) :: Arrow [Int, Int] (Arrow [Int] Int)
as == [Int]
bs == [Int, Int]
cs == [Int, Int]

  b2 sub
= flip @(rev bs ++ rev cs) . flip @bs . f . flip @(rev as) . flip @(cs ++ as)
= flip4 . flip 2 . f . flip1 . flip 3

  b2 sub (b1 sub (b0 sub halt))
= flip4 . flip 2 . f . flip1 . flip 3 . flip3 . flip2 . f . flip2 . f . $ halt
= flip4 . flip 2 . f .   id  .        id      . flip2 . f . flip2 . f . $ halt
= flip4 . flip 2 . f . flip2 . f . flip2 . f . $ flip1 halt


sub :: Arrow [Int] z -> Arrow [Int, Int] z
as == [Int]
bs == [Int, Int]
cs.length = r

  br sub
= flip^r+2 . flip^2 . sub . flip^1 . flip^r+1
= flip^r+2 . flip^2 . sub . flip^r+1

ret n :: Arrow [Int] z -> Arrow [] z
as == [Int]
bs == []
cs.length r

  br (ret n)
= flip^r . flip^0 . ret n . flip^1 . flip^r+1
= flip^r . ret n . flip^r+1

-}
