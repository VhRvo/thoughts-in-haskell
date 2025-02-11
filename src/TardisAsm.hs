{-# LANGUAGE OverloadedStrings #-}

module TardisAsm where

import Control.Monad.Tardis
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

type Address = Int

-- map label names to their addresses
type SymTable = Map Text Address

type Assembler a = Tardis SymTable SymTable a

data Instruction
  = Add
  | Mov
  | ToLabel Text
  | ToAddress Address
  | Label Text
  | Err
  deriving (Show)

-- What we want to have is a function that takes a list of Instructions
-- and returns a list of [(Addr, Instruction)] and also replace all the ToLabels with ToAddresses
-- that point to the address of the label.
-- If the label is never defined, we put an Err there.

-- Jumping to a label that is already defined is easy,
-- we look it up in our SymTable and convert ToLabel to ToAddr.
-- This sounds like an application of the State monad, doesn’t it?
-- When we encounter a label definition, just add it to the state (SymTable).

-- The problem arises from the fact that some labels might be defined after they are used.
-- The ‘else’ block of an if statement will typically be done like this.
-- Implementing this in C, you could remember these positions and at the end,
-- fill in the gaps with the knowledge you have acquired. Thunks, anyone?
runAssembler :: [Instruction] -> [(Address, Instruction)]
runAssembler asm = instructions
  where
    (instructions, _) = runTardis (assemble 0 asm) (Map.empty, Map.empty)

assemble :: Address -> [Instruction] -> Assembler [(Address, Instruction)]
assemble _ [] = pure []
assemble address (instruction : is) = case instruction of
  -- label found, update state then go on
  Label label -> do
    -- send to past
    modifyBackwards (Map.insert label address)
    -- send to future
    modifyForwards (Map.insert label address)
    -- assemble the rest of the instructions
    assemble address is
  -- jump to label found, replace with jump to address,
  -- then do the rest starting at (address + 1)
  ToLabel label -> do
    bw <- getFuture
    fw <- getPast
    -- take union of the two symbol tables
    let
      union = Map.union bw fw
      this = case Map.lookup label union of
        Just a' -> (address, ToAddress a')
        Nothing -> (address, Err)
    rest <- assemble (address + 1) is
    pure $ this : rest
  -- regular instruction found, assign it to the address,
  -- then do the rest starting at (address + 1)
  _ -> do
    rest <- assemble (address + 1) is
    pure $ (address, instruction) : rest

input :: [Instruction]
input =
  [ Add,
    Add,
    ToLabel "my_label",
    Mov,
    Mov,
    Label "my_label",
    Label "second_label",
    Mov,
    ToLabel "second_label",
    Mov
  ]

-- >> test
test :: [(Address, Instruction)]
-- [(0,Add),(1,Add),(2,ToAddress 5),(3,Mov),(4,Mov),(5,Mov),(6,ToAddress 5),(7,Mov)]
test = runAssembler input
