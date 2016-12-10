
data Direction = North | East | West | South

-- turn_clockwise: Direction -> Direction
-- turn_clockwise North = ?turn_clockwise_rhs_1
-- turn_clockwise East = ?turn_clockwise_rhs_2
-- turn_clockwise West = ?turn_clockwise_rhs_3
-- turn_clockwise South = ?turn_clockwise_rhs_4

||| this is shape
data Shape = ||| this is triangle
             Triangle Double Double
           | ||| this is rectangle
             Rectangle Double Double
           | |||
             Circle Double
area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
              | Combine Picture Picture
              | Rotate Double Picture
              | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
               (Combine (Translate 35 5 circle)
                        (Translate 15 25 triangle))


-- pictureArea : Picture -> Double
-- pictureArea (Primitive x) = ?pictureArea_rhs_1
-- pictureArea (Combine x y) = ?pictureArea_rhs_2
-- pictureArea (Rotate x y) = ?pictureArea_rhs_3
-- pictureArea (Translate x y z) = ?pictureArea_rhs_4


data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                    LT => Node (insert x left) val right
                                    EQ => orig
                                    GT => Node left val (insert x right)
{--
> insert 3 (insert 1 (insert 2 Empty))
Node (Node Empty 1 Empty)
     2
     (Node Empty 3 Empty) : Tree Integer
---}



-- Exercise

listToTree: Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x $ listToTree xs

{--
> listToTree [1,4,3,5,2]
Node (Node Empty 1 Empty)
     2
     (Node (Node Empty 3 (Node Empty 4 Empty))
           5
           Empty) : Tree Integer
--}

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ val :: treeToList right

{--
> treeToList (listToTree [4,1,8,7,2,3,9,5,6])
[1, 2, 3, 4, 5, 6, 7, 8, 9] : List Integer
--}



data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr


evaluate: Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mul x y) = evaluate x * evaluate y

{--
> evaluate (Mul (Val 10) (Add (Val 6) (Val 3)))
90 : Int
--}

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe lh@(Just x) Nothing = lh
maxMaybe lh@(Just x) rh@(Just y) = case compare x y of
                                  LT => rh
                                  EQ => rh
                                  GT => lh
{--
*4.1> maxMaybe (Just 4) (Just 5)
Just 5 : Maybe Integer
*4.1> maxMaybe (Just 4) Nothing
Just 4 : Maybe Integer
--}


testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive s) = case s of
                                     (Triangle x y) => Just $ x*y/2
                                     (Rectangle x y) => Nothing
                                     (Circle x) => Nothing
biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate x y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z
