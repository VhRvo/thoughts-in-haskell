module FormattingDemo.TextFormat where

import Data.Double.Conversion.Text
import GHC.Float
import Data.Text (Text)

float :: Float
float = 5.1875e9

-- >>> toShortest float

float1 :: Double
float1 = realToFrac float

float2 :: Double
float2 = float2Double float

float' :: Text
float' = toShortest (realToFrac float)
