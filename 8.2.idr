import Data.Vect

-- reverse : Vect n elem -> Vect n elem
-- reverse xs = ?reverse_rhs

-- myReverse : Vect n elem -> Vect n elem
-- myReverse [] = []
-- myReverse {n = S k} (x :: xs) = let
--                         rev_xs = myReverse xs
--                         result = rev_xs ++ [x]
--                       in
--                       rewrite
--                         plusCommutative 1 k
--                       in
--                         result

test : Vect 2 Int
test = 1 :: 2 :: Nil

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
  where
    reverseProof : Vect (k + 1) elem -> Vect (S k) elem
    reverseProof {k} result = rewrite plusCommutative 1 k in result



append_nil : Vect m elem -> Vect (plus m 0) elem
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs

append_xs : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs

append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)


{--
  8.2.6 Exercise
--}

-- 1. using `plusZeroRightNeutral` and `plusSuccRightSucc`, write your own shit
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = ?myPlusCommutes_rhs_1
myPlusCommutes (S k) m = ?myPlusCommutes_rhs_2

-- 2.
myReverse' : Vect n a -> Vect n a
myReverse' xs = reverse' [] xs
  where
    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' acc [] = ?reverse'_rhs_1 acc
    reverse' acc (x :: ys) = ?reverse'_rhs_2 (reverse' (x :: acc) xs)
