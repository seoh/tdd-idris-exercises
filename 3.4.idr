import Data.Vect

my_reverse : (elem : Type) -> List elem -> List elem
my_reverse elem [] = []
my_reverse elem (x :: xs) = my_reverse elem xs ++ [x]
-- my_reverse _ [1, 2]

my_reverse' : {elem : Type} -> List elem -> List elem
my_reverse' [] = []
my_reverse' (x :: xs) = xs ++ [x]
-- my_reverse' [1, 2]


my_createEmpties : Vect n (Vect 0 a)
my_createEmpties {n = Z} = []
my_createEmpties {n = (S k)} = [] :: my_createEmpties
