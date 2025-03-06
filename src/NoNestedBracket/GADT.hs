{-# LANGUAGE GADTs #-}

module NoNestedBracket.GADT where


-- data kinds
data Bracket = WithBracket | WithoutBracket

-- or :: Bool -> Bool -> Bool
-- type level function
type family Or a b :: Bracket where
    Or WithBracket _ = WithBracket
    Or _ WithBracket = WithBracket
    Or _ _ = WithoutBracket

data Type (a :: Bracket) where
    Int :: Type a
    Arrow :: Type a -> Type b -> Type (Or a b)
    Bracket :: Type WithoutBracket -> Type WithBracket

data Type0
    = Int0
    | Arrow0 Type0 Type0

data Type0F r
    = IntF0
    | ArrowF0 r r
    -- | Const Void

data Type1
    = Int1
    | Arrow1 Type1 Type1
    -- | Lift Type0
    | Bracket1 Type0 Type0

data Type1F r
    = IntF1
    | ArrowF1 r r
    -- | LiftF Type0
    | BracketF1 Type0 Type0


