module Infix where

expr :: Bool -> Bool
-- expr input = input && (let list = [1, 2, 3] in elem 3 list && let list = [2, 3, 4] in elem 4 list)
expr input = input && (let list = [1, 2, 3] in elem 3 list) && (let list = [2, 3, 4] in elem 4 list)
