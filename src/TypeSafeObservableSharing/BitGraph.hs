module TypeSafeObservableSharing.BitGraph where

import Data.Text (Text)
import Data.Text qualified as T
import TypeSafeObservableSharing.Class

data Bit
  = Xor Bit Bit
  | Delay Bit
  | Input [Bool]
  | Var Text
  deriving (Show)

xor :: Bit -> Bit -> Bit
xor = Xor

delay :: Bit -> Bit
delay = Delay

run :: (Bit -> Bit) -> [Bool] -> [Bool]
run f bs = interpret (f (Input bs))

interpret :: Bit -> [Bool]
interpret = \case
  Xor b1 b2 -> zipWith (/=) (interpret b1) (interpret b2)
  Delay b -> False : interpret b
  Input bs -> bs
  Var _ -> error "Variables are not supported"

-- Parity specification
parity :: Bit -> Bit
parity input = output
  where
    output = xor (delay output) input

demo1 :: [Bool]
demo1 = run parity (repeat True)

demo2 :: Bit
demo2 = parity (Var "x")

-- demo3 :: Graph

type Unique = Int

data BitGraph
  = BitGraph [(Unique, BitNode Unique)] Unique

data BitNode s
  = GraphXor s s
  | GraphDelay s
  | GraphInput [Bool]
  | GraphVar Text

instance MuRef Bit where
  type DeRef Bit = BitNode
  mapDeRef ::
    (Applicative f) =>
    (Bit -> f u) ->
    Bit ->
    f (BitNode u)
  mapDeRef f = \case
    Xor b1 b2 -> GraphXor <$> f b1 <*> f b2
    Delay b -> GraphDelay <$> f b
    Input bs -> pure $ GraphInput bs
    Var var -> pure $ GraphVar var

graph :: BitGraph
graph =
  BitGraph
    [ (1, GraphXor 2 3),
      (2, GraphDelay 1),
      (3, GraphVar "x")
    ]
    1
