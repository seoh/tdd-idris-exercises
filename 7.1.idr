
occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (value :: values) = case value == item of
                                          False => occurrences item values
                                          True => 1 + occurrences item values

data Matter = Solid | Liquid | Gas

-- @see https://github.com/idris-lang/Idris-dev/blob/master/libs/prelude/Prelude/Interfaces.idr#L26-L31
Eq Matter where
    (==) Solid Solid = True
    (==) Liquid Liquid = True
    (==) Gas Gas = True
    (==) _ _ = False

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
    (==) Empty Empty = True
    (==) (Node left e right) (Node left' e' right')
          = left == left' && e == e' && right == right'
    (==) _ _ = False

record Album where
       constructor MkAlbum
       artist : String
       title : String
       year : Integer

Eq Album where
    (==) (MkAlbum artist title year) (MkAlbum artist' title' year')
           = artist == artist' && title == title' && year == year'

Ord Album where
    compare (MkAlbum artist title year) (MkAlbum artist' title' year')
           = case compare artist artist' of
                  EQ => case compare year year' of
                      EQ => compare title title'
                      diff_year => diff_year
                  diff_artist => diff_artist

help : Album
help = MkAlbum "The Beatles" "Help" 1965

rubbersoul : Album
rubbersoul = MkAlbum "The Beatles" "Rubber Soul" 1965

clouds : Album
clouds = MkAlbum "Joni Mitchell" "Clouds" 1969

hunkydory : Album
hunkydory = MkAlbum "David Bowie" "Hunky Dory" 1971

heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]



-- Exercise 7.1.6

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
    (==) (Triangle x y) (Triangle x' y') = x == x' && y == y'
    (==) (Rectangle x y) (Rectangle x' y') = x == x' && y == y'
    (==) (Circle x) (Circle x') = x == x'
    (==) _ _ = False


area : Shape -> Double
area (Triangle x y) = x * y / 2
area (Rectangle x y) = x * y
area (Circle x) = x * x * pi

Ord Shape where
    compare x y = compare (area x) (area y)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4,
              Rectangle 2 7]
