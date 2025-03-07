module Infer.Test where

import Infer.ExplicitSubstitution.Infer.Manual
import Infer.Syntax

i :: Expr
i = ELambda "x" (EVariable "x")

true, false :: Expr
true = ELet "true" (ELambda "x" (ELambda "y" (EVariable "x"))) (EVariable "true")
false = ELet "false" (ELambda "x" (ELambda "y" (EVariable "y"))) (EVariable "false")

true', false' :: Expr
true' = ELambda "x" (ELambda "y" (EVariable "x"))
false' = ELambda "x" (ELambda "y" (EVariable "y"))

b, b' :: Expr
b =
  ELet
    "b"
    ( ELambda
        "x"
        ( ELambda
            "y"
            ( ELambda
                "z"
                (EApplication (EVariable "x") (EApplication (EVariable "y") (EVariable "z")))
            )
        )
    )
    (EVariable "b")
b' =
  ELambda
    "x"
    ( ELambda
        "y"
        ( ELambda
            "z"
            (EApplication (EVariable "x") (EApplication (EVariable "y") (EVariable "z")))
        )
    )

-- succ :: Expr
-- succ

main :: IO ()
main = do
  putStrLn "lambdas"
  runInfer' i >>= print
  runInfer' true' >>= print
  runInfer' false' >>= print
  runInfer' b' >>= print
  putStrLn "lambdas in let"
  runInfer' true >>= print
  runInfer' false >>= print
  runInfer' b >>= print
