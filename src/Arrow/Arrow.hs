{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Arrow.Arrow where

import Control.Arrow
import Control.Category qualified as Cat
import Control.Monad
import Data.List
import Data.Maybe
import System.Random

newtype Circuit a b
  = Circuit {unCircuit :: a -> (Circuit a b, b)}

instance Cat.Category Circuit where
  id :: Circuit a a
  id = Circuit (Cat.id,)
  (.) :: Circuit b c -> Circuit a b -> Circuit a c
  (.) = dot
    where
      (Circuit cir2) `dot` (Circuit cir1) =
        Circuit
          ( \x ->
              let
                (cir1', y) = cir1 x
                (cir2', z) = cir2 y
               in
                (cir2' `dot` cir1', z)
          )

instance Arrow Circuit where
  arr :: (a -> b) -> Circuit a b
  arr f = Circuit (\x -> (arr f, f x))
  first :: Circuit a b -> Circuit (a, c) (b, c)
  first (Circuit cir) =
    Circuit
      ( \(x, y) ->
          let (cir', z) = cir x
           in (first cir', (z, y))
      )

instance ArrowChoice Circuit where
  left :: Circuit b c -> Circuit (Either b d) (Either c d)
  left orig@(Circuit cir) = Circuit $ \case
    Left b ->
      let (cir', c) = cir b
       in (left cir', Left c)
    Right d -> (left orig, Right d)

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit cir = snd . mapAccumL unCircuit cir

-- runCircuit cir inputs = snd $ mapAccumL (\cir x -> unCircuit cir x) cir inputs
-- runCircuit cir = \case
--   [] -> []
--   x : xs ->
--     let (cir', x') = unCircuit cir x
--      in x' : runCircuit cir' xs

accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc combine =
  Circuit
    ( \input ->
        let (output, acc') = combine input acc
         in (accum acc' combine, output)
    )

accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc combine =
  Circuit
    ( \input ->
        let acc' = combine input acc
         in (accum' acc' combine, acc')
    )

total :: (Num a) => Circuit a a
total = accum' 0 (+)

-- >>> runCircuit total [1, 0, 1, 0, 0, 2]
-- [1,1,2,2,2,4]

mean1 :: (Fractional a) => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 :: (Fractional a) => Circuit a a
mean2 = proc value -> do
  t <- total -< value
  n <- total -< 1
  returnA -< t / n

generator :: (Random a) => (a, a) -> StdGen -> Circuit () a
generator range initialRng = accum initialRng $ \() rng -> randomR range rng

dictionary :: [String]
dictionary = ["dog", "cat", "bird"]

pickWord :: StdGen -> Circuit () String
pickWord rng = proc () -> do
  idx <- generator (0, length dictionary - 1) rng -< ()
  returnA -< dictionary !! idx

test :: IO ()
test = do
  rng <- getStdGen
  print $ runCircuit (pickWord rng) [(), (), ()]

oneShot :: Circuit () Bool
oneShot = accum True $ \_ acc -> (acc, False)

-- oneShot = accum' True $ \_ _ -> False

-- >>> runCircuit oneShot [(), (), ()]
-- [True,False,False]

delayedEcho :: a -> Circuit a a
delayedEcho acc = accum acc (flip (,)) -- (\a b -> (b, a))

-- >>> runCircuit (delayedEcho False) [True, False, False, False, True]

getWord :: StdGen -> Circuit () String
getWord rng = proc () -> do
  firstTime <- oneShot -< ()
  mPicked <-
    if firstTime
      then do
        picked <- pickWord rng -< ()
        returnA -< Just picked
      else
        returnA -< Nothing
  mWord <- accum' Nothing mplus -< mPicked
  returnA -< fromJust mWord
