
import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94

-- valToString : (isInt : Bool) -> StringOrInt isInt -> String
-- valToString False = trim
-- valToString True = cast

-- *6.1> valToString True 100
-- "100" : String
-- *6.1> valToString False "  a  "
-- "a" : String

valToString : (isInt : Bool) -> (case isInt of
                                      False => String
                                      True => Int) -> String
valToString False = trim
valToString True = cast
