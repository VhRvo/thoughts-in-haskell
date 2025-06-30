module Validation where

import Data.Validation
import Data.Text
import Data.Functor.Identity
import Data.Foldable

type Decl = Bool
type Error = Text

type Interpreter = Identity

checkDeclaration :: Decl -> Interpreter (Validation [Error] ())
checkDeclaration decl
    | decl = pure (pure ())
    | otherwise = pure (Failure ["Error"])

checkDeclarations :: [Decl] -> Interpreter [Validation [Error] ()]
checkDeclarations =
    traverse checkDeclaration

checkDeclarations' :: [Decl] -> Interpreter (Validation [Error] [()])
checkDeclarations' decls =
    sequenceA <$> checkDeclarations decls

checkDeclarations'1 :: [Decl] -> Interpreter (Validation [Error] ())
checkDeclarations'1 decls =
    sequenceA_ <$> traverse checkDeclaration decls

checkDeclarations'2 :: [Decl] -> Interpreter (Validation [Error] ())
checkDeclarations'2 = Data.Foldable.foldl
    (\acc decl -> (*>) <$> acc <*> decl)
    -- (\acc decl -> do
    --   accVal <- acc
    --   declVal <- decl
    --   pure (accVal *> declVal))
    (pure (Success ())) . fmap checkDeclaration

checkDeclarations'3 :: [Decl] -> Interpreter (Validation [Error] ())
checkDeclarations'3 =
    Data.Foldable.foldl (\acc decl -> do
        -- (<*>) (*>)
        accVal <- acc
        declVal <- checkDeclaration decl
        pure (accVal *> declVal)
    ) (pure (Success ()))

-- checkDeclarations'3 :: [Decl] -> Interpreter (Validation [Error] ())
-- checkDeclarations'3 = Data.Foldable.foldlM (\acc decl -> (*>) <$> acc <*> checkDeclaration decl) (pure ())

-- >>> True
demo1 = checkDeclarations'1 [True, False, True, True, False]
demo2 = checkDeclarations' [True, False, True, True, False]
demo3 = checkDeclarations [True, False, True, True, False]
demo4 = checkDeclarations'2 [True, False, True, True, False]
demo5 = checkDeclarations'3 [True, False, True, True, False]