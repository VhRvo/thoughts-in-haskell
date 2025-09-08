module Fix.Polymorphic where

polymorphicFix :: forall f. ((forall a. f a) -> (forall a. f a)) -> (forall a. f a)
polymorphicFix f =
    let
        x :: forall a. f a
        x = f x
     in x

