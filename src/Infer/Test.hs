module Infer.Test where

import Infer.Syntax
import Infer.ExplicitSubstitution.Infer.Manual

true, false :: Expr
true = ELet "true" (ELambda "x" (ELambda "y" (EVariable "x"))) (EVariable "true")
false = ELet "false" (ELambda "x" (ELambda "y" (EVariable "y"))) (EVariable "false")

true', false' :: Expr
true' = ELambda "x" (ELambda "y" (EVariable "x"))
false' = ELambda "x" (ELambda "y" (EVariable "y"))

b, b' :: Expr
b = ELet "b" (ELambda "x" (ELambda "y" (ELambda "z"
  (EApplication (EVariable "x") (EApplication (EVariable "y") (EVariable "z"))))))
  (EVariable "b")
b' = ELambda "x" (ELambda "y" (ELambda "z"
  (EApplication (EVariable "x") (EApplication (EVariable "y") (EVariable "z")))))

-- succ :: Expr
-- succ

main :: IO ()
main = do
  runInfer' true >>= print
  runInfer' true' >>= print
  runInfer' false >>= print
  runInfer' false' >>= print
  runInfer' b >>= print
  runInfer' b' >>= print

