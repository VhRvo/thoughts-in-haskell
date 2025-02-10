module Text where

import Data.Text (Text)
import qualified Data.Text as T

example1 :: IO ()
example1 = do
    print $ T.splitOn "." "abc"
    print $ T.intercalate "." $ T.splitOn "." "abc"
    print $ T.splitOn "." "abc.efg"
    print $ T.intercalate "." $ T.splitOn "." "abc.efg"
    print $ T.intercalate "." []
