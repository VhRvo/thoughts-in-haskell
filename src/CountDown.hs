module CountDown where

data Op
  = Add
  | Sub
  | Mul
  | Div

instance Show Op where
  show = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"

valid :: Op -> Int -> Int -> Bool
-- valid op x y = case op of
--   Add -> True
--   Sub -> x > y
--   Mul -> True
--   Div -> x `mod` y == 0
valid op x y = case op of
  Add -> x <= y
  Sub -> x > y
  Mul -> x /= 1 && y /= 1 && x <= y
  Div -> y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply op x y = case op of
  Add -> x + y
  Sub -> x - y
  Mul -> x * y
  Div -> x `div` y

data Expr
  = Val Int
  | App Op Expr Expr

instance Show Expr where
  show = \case
    Val n -> show n
    App o l r -> go l <> show o <> go r
    where
      go (Val n) = show n
      go expr = "(" <> show expr <> ")"

values :: Expr -> [Int]
values = \case
  Val n -> [n]
  App _ l r -> values l <> values r

-- eval :: Expr -> Maybe Int
-- eval = \case
--     Val n -> if n > 0 then Just n else Nothing
--     App o l r -> do
--         x <- eval l
--         y <- eval r
--         if valid o x y then Just (apply o x y) else Nothing

eval :: Expr -> [Int]
eval = \case
  Val n -> [n | n > 0]
  App o l r -> [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- >> sub [1, 2, 3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
subs :: [a] -> [[a]]
subs = \case
  [] -> [[]]
  x : xs -> yss <> fmap (x :) yss
    where
      yss = subs xs

-- >> interleave 1 [2,3,4]
-- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
interleave :: a -> [a] -> [[a]]
interleave x = \case
  [] -> [[x]]
  y : ys -> (x : y : ys) : fmap (y :) (interleave x ys)

-- >> perms [1,2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
perms :: [a] -> [[a]]
perms = \case
  [] -> [[]]
  x : xs -> concatMap (interleave x) (perms xs)

-- >> choices [1,2,3]
-- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
choices :: [a] -> [[a]]
choices = concatMap perms . subs

-- choices xs = [zs | ys <- subs xs, zs <- perms ys]

e' :: Expr
e' = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  -- elem (values e) (choices ns) && eval e == [n]
  values e `isChoice` ns && eval e == [n]

isChoice :: (Eq a) => [a] -> [a] -> Bool
isChoice xs ys = foldr (\x acc -> elem x ys && acc) True xs

-- isChoice [] _ = True
-- isChoice (x:xs) ys = elem x ys && isChoice xs ys

-- solve :: [Int] -> Int -> Bool
-- solve candidates target = target `elem` enumerate candidates

-- enumerate :: [Int] -> [Int]
-- enumerate = \case
--   [] -> error "unexpected empty input"
--   [number] -> [number]
--   candidate : candidates -> do
--     rest <- candidates
--     [candidate + rest, candidate * rest, candidate - rest, rest - candidate, candidate `div` rest, rest `div` candidate]

-- splitting a list into two non-empty lists that append to give the original list:
-- >> split [1, 2, 3, 4]
-- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
split :: [a] -> [([a], [a])]
split = \case
  [] -> []
  [_] -> []
  x : xs -> ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs = \case
  [] -> []
  [n] -> [Val n]
  ns -> [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e, v) <- results ns', v == n]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results = \case
  [] -> []
  [n] -> [(Val n, n) | n > 0]
  ns ->
    [ res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry
    ]

combine' :: Result -> Result -> [Result]
combine' (lE, lV) (rE, rV) = [(App o lE rE, apply o lV rV) | o <- ops, valid o lV rV]

main :: IO ()
main = print (solutions [1, 3, 7, 10, 25, 50] 765)
