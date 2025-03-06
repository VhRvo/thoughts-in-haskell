module ParserCombinator.Simple where

data Tree

-- A parser as a function that takes a string of characters as input
-- and yields some kind of tree as result.
type Parser1 = String -> Tree

-- A parser might not consume all of its input string,
-- so rather than the result of a parser being just a tree,
-- we also return the unconsumed suffix of the input string
type Parser2 = String -> (Tree, String)

-- Rather than just reporting a run-time error if this happens,
-- we choose to have parsers return a list of pairs rather than a single pair,
-- with the convention that the empty list denotes failure of a parser,
-- and a singleton list denotes success.
-- type Parser3 = String -> [(Tree, String)]

-- Different parsers will likely return different kinds of trees,
-- so it is useful to abstract on the specific type Tree of trees,
-- and make the type of result values into a parameter of the Parser type.
type Parser output = String -> [(output, String)]

-- type Parser input output = input -> [(output, input)]
