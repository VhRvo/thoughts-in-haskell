module ConstrainedType where

data Base
  = StringB
  | BoolB
  | IntB

data ConstrainedBase = CB Base Expression

newtype Expression = BoolE Bool

data Type
  = BaseT ConstrainedBase
  | ArrayT Type Expression

-- topLevel
-- this
-- self -- insert to env as value

-- Access
-- Self
-- This
