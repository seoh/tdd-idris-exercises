import Data.Vect


-- AdderType : (numargs : Nat) -> Type
-- AdderType Z = Int
-- AdderType (S k) = (next : Int) -> AdderType k

-- adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
-- adder Z acc = acc
-- adder (S k) acc = \next => adder k (next + acc)

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : Num numType =>
        (numargs : Nat) -> numType -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)

data Format = Number Format
            | Float Format
            | Str Format
            | Chr Format
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Float fmt) = (d : Double) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Chr fmt) = (ch : Char) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Float fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Chr fmt) acc = \ch => printfFmt fmt (acc ++ show ch)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Float (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt
printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""


--- Exercise

Matrix : Nat -> Nat -> Type
Matrix x y = Vect x (Vect y Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

TupleVect : (numargs : Nat) -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, TupleVect k x)

test : TupleVect 4 Nat
test = (1,2,3,4,())
