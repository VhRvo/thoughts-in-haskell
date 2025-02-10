module ReasoningAboutEffects where

import Control.Arrow

type ST s a = s -> (a, s)

assoc :: (a, (b, c)) -> ((a, b), c)
assoc (x, (y, z)) = ((x, y), z)

unAssoc :: ((a, b), c) -> (a, (b, c))
unAssoc ((x, y), z) = (x, (y, z))

cons :: (a, [a]) -> [a]
cons = uncurry (:)

append :: ([a], [a]) -> [a]
append = uncurry (<>)

(<.>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- f <.> g = curry (f . uncurry g)
(<.>) f g x y = f (g x y)

liftA1 :: (a -> b) -> ST s a -> ST s b
-- liftA1 f x = x >>> (f *** id)
liftA1 f x = x >>> first f

liftA2 :: (a -> b -> c) -> ST s a -> ST s b -> ST s c
-- liftA2 f x y = x >>> (id *** y) >>> assoc >>> (uncurry f *** id)
liftA2 f x y = x >>> second y >>> assoc >>> first (uncurry f)

liftA2' :: (a -> b -> c) -> ST s a -> ST s b -> ST s c
-- liftA2' f x y = x >>> (f *** y) >>> assoc >>> (uncurry ($) *** id)
liftA2' f x y = x >>> (f *** y) >>> assoc >>> first (uncurry ($))
