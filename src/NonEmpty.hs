module NonEmpty where

data NonEmpty a = Cons a (List a)

data List a = Empty | NonEmpty (NonEmpty a)
