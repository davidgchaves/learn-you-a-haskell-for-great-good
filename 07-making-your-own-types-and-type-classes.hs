-- Making your own types and type classes

import qualified Data.Map as Map

--
-- The custom Shape type
--

data Point = Point Float Float
    deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)
-- :t Circle    --> Circle :: Point -> Float -> Shape
-- :t Rectangle --> Rectangle :: Point -> Point -> Shape


area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- area $ Circle (Point 0 0) 10                 --> 314.15927
-- area $ Rectangle (Point 0 0) (Point 100 100) --> 10000.0


nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) xshift yshift
    = Circle (Point (x + xshift) (y + yshift)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) xshift yshift
    = Rectangle (Point (x1 + xshift) (y1 + yshift)) (Point (x2 + xshift) (y2 + yshift))
-- nudge (Circle (Point 34 34) 10) 10 20               --> Circle (Point 44.0 54.0) 10.0
-- nudge (Rectangle (Point 10 10) (Point 20 20)) 15 25 --> Rectangle (Point 25.0 35.0) (Point 35.0 45.0)


baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r
--  nudge (baseCircle 10) 30 40 -->  Circle (Point 30.0 40.0) 10.0


baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)
-- nudge (baseRectangle 40 100) 60 23 --> Rectangle (Point 60.0 23.0) (Point 100.0 123.0)


--
-- The custom Person type using Record Syntax
--

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)
-- :t flavor    --> flavor :: Person -> String
-- :t firstName --> firstName :: Person -> String
-- Person {firstName = "Buddy", lastName = "Finklestein", phoneNumber = "444-123456", flavor = "Chocolate", age = 43, height = 1.84 }
--  --> Person {firstName = "Buddy", lastName = "Finklestein", age = 43, height = 1.84, phoneNumber = "444-123456", flavor = "Chocolate"}


--
-- Playing with Type Parameters
--

data IntMaybe = INothing | IJust Int deriving (Show)
data StrMaybe = SNothing | SJust String deriving (Show)
data ShaMaybe = ShNothing | ShJust Shape deriving (Show)


--
-- To use or not to use Type Parameters
--

-- A Car with no Type Parameters
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

showCar :: Car -> String
showCar (Car { company = c, model = m, year = y }) =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
-- showCar Car { company = "Ford", model = "Mustang", year = 1967 } --> "This Ford Mustang was made in 1967"


-- A Car with Type Parameters
data Car' a b c = Car' { company' :: a
                       , model' :: b
                       , year' :: c
                       } deriving (Show)

showCar' :: (Show a) => Car' String String a -> String
showCar' (Car' { company' = c, model' = m, year' = y }) =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
-- showCar' $ Car' "Ford" "Mustang" 1967 --> "This Ford Mustang was made in 1967"

-- The Type Signature of showCar' is more convoluted that the one of showCar,
-- so maybe the Car type shouldn't be parameterized


--
-- A 3D Vector Type
--

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
-- (Vector 1 1 1) `vplus` (Vector 5 6 7) --> Vector 6 7 8

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n
-- (Vector 1 1 1) `dotProd` (Vector 5 6 7) --> 18

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)
-- (Vector 5 6 7) `vmult` 10 --> Vector 50 60 70


--
-- PhoneBook with Type Synonyms
--

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook


--
-- Lockers (uses Data.Map as Map)
--

-- LockerState data type to represent whether a locker is taken or free
data LockerState = Taken | Free
    deriving (Show, Eq)

-- Code type synonym to represent the locker code
type Code = String

-- LockerMap type synonym to represent the locker's number as a pair of LockerState and Code
type LockerMap = Map.Map Int (LockerState, Code)


-- Example of using Either a b data type
--  . errors use the Left value constructor
--  . results use the Right value constructor
-- We could have used Maybe a, but then we wouldn't know why we couldn't get the code
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber lockerMap = case Map.lookup lockerNumber lockerMap of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I"))
    ,(103,(Free, "IQSA9"))
    ,(105,(Free, "QOTSA"))
    ,(109,(Taken, "893JJ"))
    ,(110,(Taken, "99292"))
    ]
-- lockerLookup 100 lockers --> Left "Locker 100 is already taken!"
-- lockerLookup 101 lockers --> Right "JAH3I"
-- lockerLookup 102 lockers --> Left "Locker 102 doesn't exist!"


--
-- Our own List
--

-- using Cons
data List a = Empty | Cons a (List a)
    deriving (Show, Read, Eq, Ord)

-- using Cons with Record Syntax
data List' a = Empty' | Cons' { listHead :: a, listTail :: List' a }
    deriving (Show, Read, Eq, Ord)

-- using the 'fixity declaration' infixr
infixr 5 :-:
data List'' a = Empty'' | a :-: (List'' a)
    deriving (Show, Read, Eq, Ord)
-- 3 :-: 4 :-: 5 :-: Empty'' --> 3 :-: (4 :-: (5 :-: Empty''))

infixr 5 ^++
(^++) :: List'' a -> List'' a -> List'' a
Empty'' ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)
-- (3 :-: 4 :-: 5 :-: Empty'') ^++ (6 :-: 7 :-: Empty'')
--  --> 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty''))))


--
-- A binary search tree
--

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show)

-- a Tree with just 1 node
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- insert an element into a Tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- check if an element is already in a Tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

-- build a Tree using a foldr
numsTree :: Tree Integer
numsTree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]
-- Node 5
--      (Node 3
--          (Node 1 EmptyTree EmptyTree)
--          (Node 4 EmptyTree EmptyTree)
--      )
--      (Node 7
--          (Node 6 EmptyTree EmptyTree)
--          (Node 8 EmptyTree EmptyTree)
--      )


-- 8 `treeElem` numsTree --> True
-- 9 `treeElem` numsTree --> False


--
-- Anatomy of a Type Class definition
--

-- a new Type Class called Eq' is being defined
class Eq' a where
    -- functions: type declarations
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    -- functions: bodies (not mandatory)
    x === y = not (x /== y)
    x /== y = not (x === y)


--
-- The TrafficLight Data Type
--

-- TrafficLight is a Data Type, not a Type Class
data TrafficLight = Red | Yellow | Green

-- using instance for making the type instance TrafficLight of type class Eq'
-- (and some clever pattern matching)
instance Eq' TrafficLight where
    Red === Red       = True
    Yellow === Yellow = True
    Green === Green   = True
    _ === _           = False

-- using instance for making the type instance TrafficLight of type class Show
-- (and again, some pattern machine to achieve that)
instance Show TrafficLight where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"

-- Red === Red          --> True
-- Red === Green        --> False
-- [Red, Yellow, Green] --> [Red light, Yellow light, Green light]


--
-- A Yes-No Type Class (mimicking the js-like 'truthy/falsy' behaviour)
--

class YesNo a where
    yesno :: a -> Bool
-- :type yesno --> yesno :: YesNo a => a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True
-- yesno $ length [] --> False

instance YesNo [a] where
    yesno [] = False
    yesno _  = True
-- yesno ""      --> False
-- yesno "Boo"   --> True
-- yesno [0,0,0] --> True

instance YesNo Bool where
    yesno = id
-- yesno True --> True

instance YesNo (Maybe a) where
    yesno Nothing  = False
    yesno (Just _) = True
-- yesno $ Just 0 --> True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _         = True
-- yesno EmptyTree --> False

instance YesNo TrafficLight where
    yesno Red = False
    yesno _   = True
-- yesno Yellow --> True


--
-- An If statement (mimicking the js 'if' statement)
--

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal then yesResult else noResult
-- yesnoIf [] "YEAH!" "NO!"         --> "NO!"
-- yesnoIf [2,3,4] "YEAH!" "NO!"    --> "YEAH!"
-- yesnoIf True "YEAH!" "NO!"       --> "YEAH!"
-- yesnoIf (Just 500) "YEAH!" "NO!" --> "YEAH!"
-- yesnoIf Nothing "YEAH!" "NO!"    --> "NO!"


--
-- Functors!!!
--

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b


--
-- The List type is part of the Functor type class:
--      [] is a type constructor that
--          - takes one type
--          - produces types such as [Int], [String], ...
--

-- fmap' :: (a -> b) -> f a -> f b
-- map'  :: (a -> b) -> [a] -> [b]
instance Functor' [] where
    fmap' = map
-- fmap' (*2) []      --> []
-- fmap' (*2) [1,3,5] --> [2,6,10


--
-- The 'Maybe a' type is part of the Functor type class
--

-- fmap' :: (a -> b) -> f a     -> f b
-- fmap' :: (a -> b) -> Maybe a -> Maybe b
instance Functor' Maybe where
    fmap' f Nothing  = Nothing
    fmap' f (Just x) = Just (f x)
-- fmap (*2) Nothing    --> Nothing
-- fmap (*2) (Just 100) --> Just 200

-- fmap (++ " INSIDE THE JUST!") Nothing
--      --> Nothing
-- fmap (++ " INSIDE THE JUST!") (Just "Something serious.")
--      --> Just "Something serious. INSIDE THE JUST!"


--
-- The 'Tree a' type is part of the Functor type class
--

instance Functor' Tree where
    fmap' f EmptyTree           = EmptyTree
    fmap' f (Node x left right) = Node (f x) (fmap' f left) (fmap' f right)
-- fmap' (*2) EmptyTree --> EmptyTree
-- fmap' (*4) (foldr treeInsert EmptyTree [8,6,4,1,7,3,5]) -->
--      Node 20
--          (Node 12
--              (Node 4 EmptyTree EmptyTree)
--              (Node 16 EmptyTree EmptyTree)
--          )
--          (Node 28
--              (Node 24 EmptyTree EmptyTree)
--              (Node 32 EmptyTree EmptyTree)
--          )


--
-- The 'Either a b' type is part of the Functor type class
--

-- fmap' :: (a -> b) -> f a          -> f b
-- fmap' :: (b -> c) -> (Either a) b -> (Either a) c
instance Functor' (Either a) where
    fmap' f (Right x) = Right (f x)
    fmap' f (Left x)  = Left x
-- fmap' (*2) (Left "Error") --> Left "Error"
-- fmap' (*2) (Right 4)      --> Right 8

