
import Data.Vect


addRow : Num a => Vect n a -> Vect n a -> Vect n a
addRow [] [] = []
addRow (x :: xs) (y :: ys) = (x + y) :: (addRow xs ys)

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

-- transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
-- transposeHelper [] [] = []
-- transposeHelper (x:: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
-- transposeMat (x :: xs) = let xsTrans = transposeMat xs in (transposeHelper x xsTrans)
transposeMat (x :: xs) = let xsTrans = transposeMat xs in (zipWith (::) x xsTrans)

addMatrix : Num a =>
            Vect rows (Vect cols a) -> Vect rows (Vect cols a) ->
            Vect rows (Vect cols a)
addMatrix [] [] = []
-- addMatrix (x :: xs) (y :: ys) = (zipWith addRow x y) :: addMatrix xs ys
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMatrix xs ys


-- multRow : Num a => Vect n a -> Vect n a -> a
-- multRow [] [] = 0
-- multRow (x :: xs) (y :: ys) = (x * y) + (multRow xs ys)
-- sum $ zipWith (*) xs ys

multMatrix' : Num a => Vect n a -> Vect m (Vect n a) -> Vect m a
multMatrix' _ [] = []
-- multMatrix' x (y :: ys) = (multRow x y) :: multMatrix' x ys
multMatrix' x (y :: ys) = (sum $ zipWith (*) x y) :: multMatrix' x ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix []        _  = []
multMatrix xs ys = let ysTrans = transposeMat ys in
                   map (\x => multMatrix' x ysTrans) xs
