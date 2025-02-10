module HigherRankDefunctionalization where

-- coolThing :: (???) -> ([Integer], [String])
-- coolThing perm = (perm [0, 1], perm ["Higher", "Rank", "Polymorphism"])

data Permutation
    = Id
    | Rev
    | Rot Int
    | Permutation :>>> Permutation

permute :: forall a. Permutation -> [a] -> [a]
permute perm = case perm of
    Id -> id
    Rev -> reverse
    Rot n -> \xs -> let (ys, zs) = splitAt n xs in zs <> ys
    first :>>> second -> permute second . permute first

coolThing :: Permutation -> ([Integer], [String])
coolThing p = (permute p [0, 1], permute p ["Higher", "Rank", "Polymorphism"])
