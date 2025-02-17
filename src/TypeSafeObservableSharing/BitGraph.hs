module TypeSafeObservableSharing.BitGraph where

import Data.Text (Text)
import qualified Data.Text as T

type Unique = Int

data BitGraph
    = BitGraph [ (Unique, BitNode Unique)] Unique

data BitNode s
  = GraphXor s s
  | GraphDelay s
  | GraphInput [Bool]
  | GraphVar Text

