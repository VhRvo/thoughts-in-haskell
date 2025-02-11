-- https://stackoverflow.com/questions/38904378/duplicate-edsl-code-generated-with-recursivedo-in-haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Asm where

import Control.Monad.Fix
import Control.Monad.RWS
import Data.Foldable qualified as F
import Data.Sequence qualified as S
import Data.Word (Word16)
import Text.Printf (printf)

i# :: (Integral a, Num b) => a -> b
i# = fromIntegral

-- ================================================================= Assembler

data Instruction
  = END
  | CLEARVER
  | SET Operand Operand
  | CMP Operand Operand
  | AND Operand Operand
  | OR Operand Operand
  | NOT Operand
  | JMP Operand
  | JE Operand
  | JNE Operand
  | JG Operand
  | JGE Operand
  | JB Operand
  | JBE Operand
  | ADD Operand Operand
  | SUB Operand Operand
  | RETURN
  | CALLID Operand
  | PLAYOID Operand
  | PAUSE Operand
  {- … -}
  deriving (Show)

data AsmState = AsmState
  { _code :: [Instruction],
    _location :: Location,
    _codeHistory :: [([Instruction], [Instruction])]
  }

deAsmCode :: [Instruction] -> Int -> [String]
deAsmCode [] _ = ["[]"]
deAsmCode code pc = zipWith deAsm1 [0 ..] code
  where
    deAsm1 :: Int -> Instruction -> String
    deAsm1 addr instr = printf "%04X:%s %s" addr (pointer addr) (show instr)
    pointer :: Int -> String
    pointer addr = if addr == i# pc then ">" else " "

instance Show AsmState where
  show (AsmState {..}) =
    "AsmState {"
      ++ unlines
        [ "Code:\n" ++ unlines (deAsmCode _code 0),
          "Location: " ++ show _location,
          "History:\n" ++ unlines (map deAsmHistory _codeHistory)
        ]
      ++ "}"
    where
      deAsmHistory (a, b) =
        unlines $
          deAsmCode a 0
            ++ ["++"]
            ++ deAsmCode b 0

{-
instance Monoid AsmState where
    mempty = AsmState
                { _code = []
                , _location = A 0
                , _codeHistory = []
                }
    mappend AsmState{c1 = _code, h1 = _codeHistory} AsmState{c2 = _code, l2 = _location, h2 = _codeHistory} =
        AsmState { _code = c1 <> c2, _location = l2, _codeHistory = h1 <> h2 }

-}

newtype Assembler a = Assembler (RWS () (S.Seq Instruction) Word16 a)
  deriving (Functor, Applicative, Monad, MonadFix)

{- Append the list of instructions to the code stream. -}
append :: [Instruction] -> Assembler ()
append xs = Assembler . rws $ \_ loc -> ((), loc + (i# . length $ xs), S.fromList xs)

-- ========================================================= Instructions

data Operand
  = R Word16 -- registers
  | I Word16 -- immediate value (integer)
  | A Word16 -- address (location)
  deriving (Eq, Show)

type Location = Operand

-- Instructions
class Instructions m where
  end :: m ()
  clearVer :: m ()
  set :: Operand -> Operand -> m ()
  cmp :: Operand -> Operand -> m ()
  and :: Operand -> Operand -> m ()
  or :: Operand -> Operand -> m ()
  not :: Operand -> m ()
  jmp :: Location -> m ()
  je :: Location -> m ()
  jne :: Location -> m ()
  jg :: Location -> m ()
  jge :: Location -> m ()
  jb :: Location -> m ()
  jbe :: Location -> m ()
  add :: Operand -> Operand -> m ()
  sub :: Operand -> Operand -> m ()
  ret :: m ()
  callid :: Operand -> m ()
  playoid :: Operand -> m ()
  pause :: Operand -> m ()

  label :: m Location

{- Code combinators -}
repeatN :: (MonadFix m, Instructions m) => Operand -> m a -> m a
repeatN n@(R _) body = mdo
  _loop <- label
  cmp n (I 0)
  je _end
  retval <- body
  sub n (I 1)
  jmp _loop
  _end <- label
  return retval
repeatN _ _ = undefined

{-
    Derived (non-native) instructions, aka macros
    Scratch registers r70..r79
-}
shl :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
shl r@(R _) bits = mdo
  -- allocate registers
  let count = R 70

  set count bits
  repeatN count $ mdo
    add r r -- shift left by one
shl _ _ = undefined

instance Instructions Assembler where
  end = append [END]
  clearVer = append [CLEARVER]
  set op1 op2 = append [SET op1 op2]
  cmp op1 op2 = append [CMP op1 op2]
  and op1 op2 = append [AND op1 op2]
  or op1 op2 = append [OR op1 op2]
  not op1 = append [NOT op1]

  jmp op1 = append [JMP op1]
  je op1 = append [JE op1]
  jne op1 = append [JNE op1]
  jg op1 = append [JG op1]
  jge op1 = append [JGE op1]
  jb op1 = append [JB op1]
  jbe op1 = append [JBE op1]

  add op1 op2 = append [ADD op1 op2]
  sub op1 op2 = append [SUB op1 op2]

  ret = append [RETURN]
  callid op1 = append [CALLID op1]
  playoid op1 = append [PLAYOID op1]
  pause op1 = append [PAUSE op1]

  {- The label function returns the current index of the output stream. -}
  label = A <$> Assembler get

-- ========================================================= Tests

asm :: Assembler () -> AsmState
asm (Assembler proc) =
  let (location, code) = execRWS proc () 0
   in AsmState
        { _code = F.toList code,
          _location = A location,
          _codeHistory = []
        }

doTest :: Assembler () -> String -> IO ()
doTest proc testName = do
  let ass = asm proc
  putStrLn testName
  print ass

proc1 :: Assembler ()
proc1 = mdo
  set (R 0) (I 0x5a5a)
  shl (R 0) (I 1)
  end

proc2 :: Assembler ()
proc2 = mdo
  set (R 0) (I 0x5a5a)
  -- allocate registers
  let r = R 0
  let bits = I 2
  let count = R 70

  set count bits
  _loop <- label
  cmp count (I 0)
  je _end
  add r r
  sub count (I 1)
  jmp _loop
  _end <- label
  end

-- ========================================================= Main

main :: IO ()
main = do
  doTest proc1 "Incorrect Output"
  doTest proc2 "Correct Output"
