module ReadMaybeParser where

-- import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void (Void)
import Text.Read
import qualified Data.Set as Set

data A = Aaa
    deriving (Read, Show)
data B = Bbb
    deriving (Read, Show)
data C = A A | B B
    deriving (Read, Show)

type Parser = Parsec Void String

parseString :: Parser String
parseString = takeWhileP Nothing (/= '"')

parseA :: Parser A
parseA = do
    -- offset <- getOffset
    -- input <- getInput
    try $ readMaybe <$> parseString >>= \case
        Nothing -> do
            -- setOffset offset
            -- setInput input
            -- parseError $ FancyError 0 (Set.singleton (ErrorFail "can not read into A"))
            fail "unexpected A"
--             pFail :: String -> ParsecT e s m a
-- pFail msg = ParsecT $ \s@(State _ o _ _) _ _ _ eerr ->
--   let d = E.singleton (ErrorFail msg)
--    in eerr (FancyError o d) s
            -- fail "unexpected A"
            -- unexpected "A"
        Just value -> pure value

parseB :: Parser B
parseB = do
    readMaybe <$> parseString >>= \case
        Nothing -> pure Bbb
        Just value -> pure value

parseC :: Parser C
parseC = do
    A <$> parseA <|> B <$> parseB

test :: IO ()
test = parseTest parseC "Bbb"









