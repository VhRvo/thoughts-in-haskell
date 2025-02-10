module HigherRankInStandard where

-- coolThing :: (???) -> ([Integer], [String])
-- coolThing perm = (perm [0, 1], perm ["Higher", "Rank", "Polymorphism"])

class Permutation p where
    permute :: forall a. p -> [a] -> [a]

data Id = Id
instance Permutation Id where
    permute Id = id

data Rev = Rev
instance Permutation Rev where
    permute Rev = reverse

newtype Rot = Rot Int
instance Permutation Rot where
    permute (Rot n) xs = zs <> ys
      where
        (ys, zs) = splitAt n xs

data Com p q = p :>>> q
instance (Permutation p, Permutation q) => Permutation (Com p q) where
    permute (p :>>> q) = permute q . permute p

coolThing :: Permutation p => p -> ([Integer], [String])
coolThing p = (permute p [0, 1], permute p ["Higher", "Rank", "Polymorphism"])

examples :: IO ()
examples = do
    print $ coolThing Id
    print $ coolThing Rev
    print $ coolThing (Rot 2 :>>> Rev)
    print $ coolThing (Rev :>>> Rot 1)
