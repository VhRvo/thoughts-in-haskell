{-# LANGUAGE TupleSections #-}

module Megaparsec.IndentationSensitive where

import Control.Applicative hiding (some)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char hiding (newline)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItemList :: Parser (String, [(String, [String])]) -- header and list items
-- pItemList :: Parser (String, [String]) -- header and list items
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      pure (L.IndentSome Nothing (pure . (header,)) pComplexItem)

-- pure (L.IndentMany Nothing (pure . (header, )) pItem)
-- pure (L.IndentMany (Just (mkPos 5)) (pure . (header, )) pItem)

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      -- pure (L.IndentMany Nothing (pure . (header, )) pItem)
      pure (L.IndentMany Nothing (pure . (header,)) pLineFold)

pLineFold :: Parser String
pLineFold = L.lineFold scn $ \sc' ->
  let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
   in unwords <$> ps <* scn

newline :: IO ()
newline = putStrLn ""

main :: IO ()
main = do
  -- parseTest (pItemList <* eof) ""
  -- parseTest (pItemList <* eof) "something"
  -- newline
  -- parseTest (pItemList <* eof) "\nsomething"
  -- newline
  -- parseTest (pItemList <* eof) "  something"
  -- newline
  -- parseTest (pItemList <* eof) "something\n"
  -- parseTest (pItemList <* eof) "something\none\ntwo\nthree"
  -- parseTest (pItemList <* eof) "something\n  one\n  two\n  three"
  parseTest
    (pItemList <* eof)
    "first-chapter\n\
    \  paragraph-one\n\
    \      note-A # an important not here!\n\
    \      note-B\n\
    \  paragraph-two\n\
    \    note-1\n\
    \    note-2\n\
    \  paragraph-three\n"
