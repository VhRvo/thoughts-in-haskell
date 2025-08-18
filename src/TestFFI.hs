{-# LANGUAGE ForeignFunctionInterface #-}

module TestFFI where

import Foreign.C.Types

-- fibonacci :: Int -> Int
-- fibonacci n = fibs !! n
--     where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- fibonacci_hs :: CInt -> CInt
-- fibonacci_hs = fromIntegral . fibonacci . fromIntegral

-- foreign export ccall fibonacci_hs :: CInt -> CInt

-- import Data.Function.FFI

-- fact :: Int -> Int
-- fact 0 = 1
-- fact n = n * fact (n - 1)

-- fact_hs :: CInt -> CInt
-- fact_hs = fromIntegral . fact . fromIntegral
-- foreign export ccall fact_hs :: CInt -> CInt
