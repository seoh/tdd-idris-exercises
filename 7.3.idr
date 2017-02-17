totalLen : List String -> Nat
-- totalLen xs = foldr (\str, len => length str + len) 0 xs
totalLen xs = sum $ map length xs

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
Functor Tree where
    map func Empty = Empty
    map func (Node left e right)
        = Node (map func left)
               (func e)
               (map func right)

Foldable Tree where
    foldr func acc Empty = acc
    foldr func acc (Node left e right)
         = let leftfold = foldr func acc left
               rightfold = foldr func leftfold right in
               func e rightfold


-- Exercise 1.

-- from 7.2.idr <-!!
data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub
    abs = Abs
-- from 7.2.idr !->>

Functor Expr where
    map func (Val x) = Val $ func x
    map func (Add x y) = Add (map func x) (map func y)
    map func (Sub x y) = Sub (map func x) (map func y)
    map func (Mul x y) = Mul (map func x) (map func y)
    map func (Div x y) = Div (map func x) (map func y)
    map func (Abs x) = Abs $ map func x


-- Exercise 2.
data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

Eq a => Eq (Vect n a) where
    (==) [] [] = True
    (==) (x :: xs) (y :: ys) = (x == y) && (xs == ys)

Foldable (Vect n) where
    foldr func acc Nil = acc
    foldr func acc (x :: xs) = func x (foldr func acc xs)
