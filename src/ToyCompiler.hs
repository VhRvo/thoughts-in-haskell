{-# LANGUAGE StrictData #-}

module ToyCompiler where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT, execWriterT, tell)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
-- import Text.ParserCombinators.UU (pChainl)
-- import Text.ParserCombinators.UU.Utils
import Text.PrettyPrint.Leijen
  ( comma,
    hsep,
    int,
    punctuate,
    text,
    vsep,
    (<+>),
    (<>),
  )

data Expr
  = Lit Int
  | Add Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval = \case
  Lit number -> number
  Add left right -> eval left + eval right

example :: Expr
example = Add (Add (Lit 1) (Add (Lit 2) (Lit 3))) (Lit 4)

type Program = [Instruction]

ppProg :: Program -> String
ppProg = show . vsep . map ppInst
  where
    ppInst (IAdd r o1 o2) = text "IAdd" <+> ppOperands [Reg r, o1, o2]
    ppOperands = hsep . punctuate comma . map ppOpd
    ppOpd (Immediate i) = int i
    ppOpd (Reg r) = text "%" <> int r

data Instruction
  = IAdd Reg Operand Operand
  deriving (Show)

data Operand
  = Immediate Int
  | Reg Reg
  deriving (Show)

type Reg = Int

newtype CodeGen a = CodeGen (WriterT [Instruction] (State Reg) a)
  deriving newtype (Functor, Applicative, Monad, MonadWriter [Instruction], MonadState Reg)

parse :: Text -> Expr
parse = undefined

codeGen :: Expr -> CodeGen Operand
codeGen = \case
  Lit number -> pure $ Immediate number
  Add left right -> do
    o1 <- codeGen left
    o2 <- codeGen right
    r <- newReg
    tell [IAdd r o1 o2]
    pure $ Reg r

newReg :: CodeGen Reg
newReg = modify' succ >> get

runCodeGen :: forall a. CodeGen a -> Program
runCodeGen (CodeGen m) = (`evalState` 0) . execWriterT $ m

type VM = State (Map Reg Int)

exec :: Instruction -> VM ()
exec = \case
  IAdd r o1 o2 -> do
    v1 <- load o1
    v2 <- load o2
    store r $ v1 + v2

load :: Operand -> VM Int
load = \case
  Immediate number -> pure number
  Reg r -> gets $ M.findWithDefault 0 r

store :: Reg -> Int -> VM ()
store r v = modify' (M.insert r v)

run :: Program -> Map Reg Int
run = (`execState` M.empty) . traverse_ exec
