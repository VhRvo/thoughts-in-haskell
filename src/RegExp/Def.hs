module RegExp.Def where

data RegExp
    = Zero
    | One
    | Char Char
    | Cat RegExp RegExp
    | Sum RegExp RegExp
    | Star RegExp

language :: RegExp -> [String]
language = \case
  Zero -> []
  One -> [""]
  Char ch -> [[ch]]
  Cat r1 r2 -> cat (language r1) (language r2)
  Sum r1 r2 -> language r1 <> language r2
  Star r1 -> star (language r1)

cat :: [String] -> [String] -> [String]
cat l1 l2 = (<>) <$> l1 <*> l2

star :: [String] -> [String]
star language = go [] [""]
  where
    go sum product =
      let product' = cat language product
       in go (sum <> product') product'

accept :: RegExp -> String -> [String]
accept regExp input =
  case regExp of
    Zero -> []
    One -> [input]
    (Char ch) ->
      case input of
        [] -> []
        ch' : rest
          | ch == ch' -> [rest]
          | otherwise -> []
    Cat r1 r2 -> do
      rest <- accept r1 input
      accept r2 rest
    Sum r1 r2 ->
      accept r1 input <> accept r2 input
    Star r1 -> acceptStar r1 input

acceptStar :: RegExp -> String -> [String]
acceptStar regExp input =
  let result = accept regExp input
   in (do
    rest <- result
    acceptStar regExp rest) <> [input]

re1 :: RegExp
re1 = Cat (Char 'a') (Cat (Char 'b') (Star (Cat (Char 'b') (Char 'a'))))

-- >>> (accept re1 "abbaba")
-- ["","ba","baba"]
