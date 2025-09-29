# cps

append xs ys =
  case xs of
    [] -> ys
    x : xs' -> x : append xs' ys

appendK xs ys k = k (append xs ys)

appendK xs ys k =
  case xs of
    [] -> k ys
    x : xs' -> k (x : append xs' ys)

appendK xs ys k =
  case xs of
    [] -> k ys
    x : xs' -> appendK xs' ys (k . (x :))

# cont

append xs ys =
  case xs of
    [] -> ys
    x : xs' -> x : append xs' ys

append xs ys =
  case xs of
    [] -> \empty cons -> ys empty cons
    x : xs' -> \empty cons -> (x : append xs' ys) empty cons

append xs ys =
  case xs of
    [] -> \empty cons -> ys empty cons
    x : xs' -> \empty cons -> cons x (append xs' ys)

## 1
append xs ys =
  case xs of
    [] -> \empty cons -> ys empty cons
    x : recursion -> \empty cons -> cons x recursion

append xs ys =
  xs
    (\empty cons -> ys empty cons)
    (\x recursion -> \empty cons -> cons x recursion)

## 2

append xs ys empty cons =
  case xs of
    [] -> ys empty cons
    x : xs' -> cons x (append xs' ys empty cons)

append xs ys empty cons =
  foldr
    (ys empty cons)
    cons
    xs

ap xs ys =
  case xs of
    [] -> []
    x : xs' ->
      case ys of
        [] -> []
        y : ys' -> x y : ap xs' ys'

ap xs =
  case xs of
    [] -> \ys -> []
    x : xs' ->
      \ys ->
      case ys of
        [] -> []
        y : ys' -> x y : ap xs' ys'

ap xs =
  foldr
    (\ys -> [])
    (\x recursion ->
        \ys ->
        case ys of
            [] -> []
            y : ys' -> x y : recursion ys')
    xs

# bind

ap fs xs =
  bind fs (\f ->
    map f xs)

bind xs f =
  case xs of
    [] -> []
    x : xs' -> f x <> bind f xs'

bind xs f =
  foldr
    []
    (\x recursion ->
      f x <> recursion)
    xs

bind xs f =
  case xs of
    [] -> []
    x : xs' -> f x <> bind f xs'

bind xs f empty cons =
  case xs of
    [] -> [] empty cons
    x : xs' -> (f x <> bind f xs') empty cons

bind xs f empty cons =
  foldr
    ([] empty cons)
    (\x recursion ->
      (append (f x) recursion empty cons)
    xs

append xs ys empty cons =
  foldr
    (ys empty cons)
    cons
    xs

bind xs f empty cons =
  foldr
    ([] empty cons)
    (\x recursion ->
      (foldr (recursion empty cons) cons xs) xs

bind xs f empty cons =
  foldr
    empty
    (\x recursion ->
      (foldr (recursion empty cons) cons xs)) xs

bind xs f empty cons =
  xs
    empty
    (\x recursion ->
      (xs (recursion empty cons) cons))