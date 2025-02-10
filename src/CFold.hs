module CFold where

cFold :: (b -> a -> a) -> a -> [b] -> a
cFold f zero list = cFold' (\x acc k -> k (f x acc)) zero list

cFold' :: (b -> a -> (a -> a) -> a) -> a -> [b] -> a
cFold' f zero = \case
    [] -> zero
    x:xs -> f x zero (\x' -> cFold' f x' xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl = undefined

foldr :: (b  -> a  -> a)  -> a  -> [b]  -> a
foldr = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined
