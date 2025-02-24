module TypeSafeObservableSharing.StableName where

import GHC.StableName

main :: IO ()
main = do
  let int = (1 + 2 + 3 + 4) :: Int
  sn1 <- makeStableName int
  print int
  sn2 <- makeStableName int -- (int `seq` int)
  print (sn1 == sn2) -- True
--   let int2 = 1 :: Int
--   sn' <- makeStableName int2
--   print (sn1 == sn') -- False

