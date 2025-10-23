module FormattingDemo.Demo where

import qualified Formatting as F
import Formatting.Examples
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import Data.Scientific
import Text.Printf (printf)

float :: Float
float = 5.1875e9

double :: Double
double = 5.1875e9

sci :: Scientific
sci = 5.1875e9

main :: IO ()
main = do
  T.putStrLn floats
  print float
  print double
  print sci
  print (toRealFloat @Float sci)
  print (toRealFloat @Double sci)

-- >>> (F.format (F.float) (float))
-- >>> (F.format (F.float) (double))
-- "5187500032"
-- "5187500000"

sciMain :: IO ()
sciMain = do
  T.putStrLn (F.format (F.float) (float))
  T.putStrLn (F.format (F.float) (double))

-- >>>  printf @(Float -> String) "%f\n" 0.123456789101112131415
-- >>>  printf @(Double -> String) "%f\n" 0.123456789101112
-- >>>  printf @(Float -> String) "%f\n" float
-- >>>  printf @(Double -> String) "%f\n" double
-- "0.12345679\n"
-- "0.123456789101112\n"
-- "5187500000.0\n"
-- "5187500000.0\n"

printfMain :: IO ()
printfMain = do
  printf "%f\n" float
  printf "%f\n" double
