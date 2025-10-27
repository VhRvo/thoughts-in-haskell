{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Calculating.Compiler.Complicated.NormalFirstStack where

data Exp
  = Val Int
  | Bin Op Exp Exp

data Op
  = Add
  | Mul

applyOp :: Op -> Int -> Int -> Int
applyOp Add = (+)
applyOp Mul = (*)

applyOpK :: Op -> Int -> Int -> (Int -> Int) -> Int
applyOpK Add l r k = k (l + r)
applyOpK Mul l r k = k (l * r)

eval :: Exp -> Int
eval =
  \case
    Val n -> n
    Bin op x y -> applyOp op (eval x) (eval y)

type Stack = [Int]

push :: Int -> Stack -> Stack
push = (:)

bin :: Op -> Stack -> Stack
bin op (n : m : s) = applyOp op m n : s
bin _ _ = error "compiler bug"

evalS :: Exp -> Stack -> Stack
evalS exp s =
  case exp of
    Val n -> push n s
    Bin op x y ->
      bin op (evalS y (evalS x s))

type SCont = Stack -> Stack

evalSK' :: Exp -> SCont -> SCont
evalSK' exp k =
  case exp of
    Val n -> k . push n
    Bin op x y ->
      evalSK' x (evalSK' y (k . bin op))

evalSK :: Exp -> Stack -> SCont -> Stack
evalSK exp s k =
  case exp of
    Val n -> k (push n s)
    Bin op x y ->
      evalSK x s (\sx ->
        evalSK y sx (\sy ->
          k (bin op sy)))

data DSCont1
  = DSId1
  | Push1 Int DSCont1
  | Bin1 Op DSCont1

evalSK'D :: Exp -> DSCont1 -> DSCont1
evalSK'D exp k =
  case exp of
    Val n -> Push1 n k
    Bin op x y ->
      evalSK'D x (evalSK'D y (Bin1 op k))

data DSAction1
  = PushAction1 Int
  | BinAction1 Op

evalSK'DA :: Exp -> [DSAction1] -> [DSAction1]
evalSK'DA exp k =
  case exp of
    Val n -> PushAction1 n : k
    Bin op x y ->
      evalSK'DA x (evalSK'DA y (BinAction1 op : k))

evalSK'DAK' :: Exp -> [DSAction1] -> ([DSAction1] -> [DSAction1]) -> [DSAction1]
evalSK'DAK' exp k k'=
  case exp of
    Val n -> k' (PushAction1 n : k)
    Bin op x y ->
      -- k' (evalSK'DA x (evalSK'DA y (BinAction1 op : k)))
      -- evalSK'DAK y (BinAction1 op : k) (\sy' -> k' (evalSK'DA x sy'))
      evalSK'DAK' y (BinAction1 op : k) (\sy' -> evalSK'DAK' x sy' k')

evalSK'DAK :: Exp -> ([DSAction1] -> [DSAction1]) -> [DSAction1] -> [DSAction1]
evalSK'DAK exp k' k=
  case exp of
    Val n -> k' (PushAction1 n : k)
    Bin op x y ->
      evalSK'DAK y (evalSK'DAK x k') (BinAction1 op : k)

data DDSCont1
  = DDSId1
  | DDSCons1 Exp DDSCont1

applyDDSCont1 :: DDSCont1 -> [DSAction1] -> [DSAction1]
applyDDSCont1 DDSId1 = id
applyDDSCont1 (DDSCons1 x k') = evalSK'DAKD x k'

evalSK'DAKD :: Exp -> DDSCont1 -> [DSAction1] -> [DSAction1]
evalSK'DAKD exp k' k=
  case exp of
    Val n -> applyDDSCont1 k' (PushAction1 n : k)
    Bin op x y ->
      evalSK'DAKD y (DDSCons1 x k') (BinAction1 op : k)

newtype DDSAction1
  = DDSConsAction1 Exp

applyDDSACont1 :: [DDSAction1] -> [DSAction1] -> [DSAction1]
applyDDSACont1 [] = id
applyDDSACont1 (DDSConsAction1 x : k') = evalSK'DAKDA x k'

evalSK'DAKDA :: Exp -> [DDSAction1] -> [DSAction1] -> [DSAction1]
evalSK'DAKDA exp k' k=
  case exp of
    Val n -> applyDDSACont1 k' (PushAction1 n : k)
    Bin op x y ->
      evalSK'DAKDA y (DDSConsAction1 x : k') (BinAction1 op : k)

_ = ()
  where
    applyDSACont1 :: [DSAction1] -> Stack -> Stack
    applyDSACont1 [] s = s
    applyDSACont1 (PushAction1 n : rest) s = applyDSACont1 rest (n : s)
    applyDSACont1 (BinAction1 op : rest) s = applyDSACont1 rest (bin op s)

    -- applyDDSACont1 :: [DDSAction1] -> [DSAction1] -> Stack -> Stack
    -- applyDDSACont1 [] = applyDSACont1
    -- applyDDSACont1 (DDSConsAction1 x : k') = evalSK'DAKDA x k'

    -- evalSK'DAKDA :: Exp -> [DDSAction1] -> [DSAction1] -> Stack -> Stack
    -- evalSK'DAKDA exp k' k s =
    --   case exp of
    --     Val n -> applyDDSACont1 k' (PushAction1 n : k) s
    --     Bin op x y ->
    --       evalSK'DAKDA y (DDSConsAction1 x : k') (BinAction1 op : k) s
    -- evalSK'DAKDA :: Exp -> [DDSAction1] -> [DSAction1] -> Stack -> Stack
    -- evalSK'DAKDA exp k' k s =
    --   case exp of
    --     Val n ->
    --       case k' of
    --         [] -> applyDSACont1 (PushAction1 n : k) s
    --         (DDSConsAction1 x : k') -> evalSK'DAKDA x k' (PushAction1 n : k) s
    --     Bin op x y ->
    --       evalSK'DAKDA y (DDSConsAction1 x : k') (BinAction1 op : k) s

    evalSK'DAKDA :: Exp -> [DDSAction1] -> [DSAction1] -> Stack -> Stack
    evalSK'DAKDA exp k' k s =
      case exp of
        Val n ->
          case k' of
            [] -> applyDSACont1 k (n : s)
            (DDSConsAction1 x : k') -> evalSK'DAKDA x k' (PushAction1 n : k) s
        Bin op x y ->
          evalSK'DAKDA y (DDSConsAction1 x : k') (BinAction1 op : k) s


data DSCont2
  = Id2
  | Push2 Int DSCont2
  | Bin2 Op DSCont2

evalK :: Exp -> (Int -> Int) -> Int
evalK exp k =
  case exp of
    Val n -> k n
    Bin op x y -> evalK x (\x' -> evalK y (\y' -> applyOpK op x' y' k))

data Cont
  = Id
  | OBin Op Exp Cont
  | IBin Op Int Cont

applyCont :: Cont -> Int -> Int
applyCont =
  \case
    Id -> id
    OBin op y k -> \x' -> evalDefun y (IBin op x' k)
    IBin op x' k -> \y' -> applyCont k (applyOp op x' y')

evalDefun :: Exp -> Cont -> Int
evalDefun exp k =
  case exp of
    Val n -> applyCont k n
    Bin op x y -> evalDefun x (OBin op y k)

data Action'
  = OBin' Op Exp
  | IBin' Op Int

applyCont1 :: [Action'] -> Int -> Int
applyCont1 =
  \case
    [] -> id
    OBin' op y : k -> \x' -> evalDefun1 y (IBin' op x' : k)
    IBin' op x' : k -> \y' -> applyCont1 k (applyOp op x' y')

evalDefun1 :: Exp -> [Action'] -> Int
evalDefun1 exp k =
  case exp of
    Val n -> applyCont1 k n
    Bin op x y -> evalDefun1 x (OBin' op y : k)

data Tag
  = TExp Exp
  | TInt Int

step :: ([Action'], Tag) -> ([Action'], Tag)
step ([], TInt n)                =  ([], TInt n)
step (OBin' op y  : k, TInt x')  =  step (IBin' op x' : k, TExp y)
step (IBin' op x' : k, TInt y')  =  step (k, TInt (applyOp op x' y'))
step (k, TExp (Val n))           =  step (k, TInt n)
step (k, TExp (Bin op x y))      =  step (OBin' op y : k, TExp x)

-- applyCont2 :: [Action'] -> Int -> [Int] -> [Int]
-- applyCont2 =
--   \case
--     [] -> (:)
--     OBin' op y : k -> \x' -> evalDefun2 y (IBin' op x' : k)
--     IBin' op x' : k -> \y' -> applyCont2 k (applyOp op x' y')

-- evalDefun2 :: Exp -> [Action'] -> [Int] -> [Int]
-- evalDefun2 exp k =
--   case exp of
--     Val n -> applyCont2 k n
--     Bin op x y -> evalDefun2 x (OBin' op y : k)
