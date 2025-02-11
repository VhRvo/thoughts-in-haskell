module SimpleRustSyntax where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show)

-- -- data Pattern

-- type Parser = Parsec Void Text

-- data Expr
--     -- w/o block
--     = Variable Identifier
--     | Binary Operator Expr Expr
--     | Application Expr [Expr]
--     | Lambda [Identifier] Expr
--     | Tuple [Expr]
--     | OperatorExpr OperatorExpr
--     | NegationExpr NegationExpr
--     -- with block
--     | Block [Stmt] (Maybe Expr)
--     | If Expr Expr Expr
--     | For Identifier Expr Expr
--     | Loop Expr Expr

-- data Stmt
--     = Let Identifier Expr
--     | Expr Expr

-- data Block = Block [Stmt] (Maybe Expr)

-- data Expr
--     = ExprWithBlock ExprWithBlock
--     | ExprWithoutBlock ExprWithoutBlock

-- data ExprWithoutBlock
--     = Variable Identifier
--     | Binary Operator Expr Expr
--     | Application Expr [Expr]
--     | Lambda [Identifier] Block
--     | Tuple [Expr]
--     -- = OperatorExpr OperatorExpr
--     -- | NegationExpr NegationExpr

-- data Operator = Add | Mul

-- -- data OperatorExpr
--     -- = OperatorExpr

-- data ExprWithBlock
--     = BlockExpr Block
--     | If Expr Block Block
--     | For Identifier Expr Block
--     | Loop Expr Block

-- data Block
--     = Block [Statement] Expr
--     -- = Block [Statement] (Maybe Expr)
--     -- = Block [Statement] (Maybe ExprWithoutBlock)

-- data Statement
--     = Let Identifier Expr
--     | ExpressionStatement ExpressionStatement

-- data ExpressionStatement
--     = ExprWithBlockStmt ExprWithBlock
--     | ExprWithoutBlockStmt ExprWithoutBlock
