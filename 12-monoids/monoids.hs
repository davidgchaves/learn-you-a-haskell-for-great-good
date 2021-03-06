--
-- The Monoid Type Class (from Data.Monoid)
--

-- Only concrete types can be made instances of Monoid,
-- because the m in the type class definition doesn't take any type parameters.
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty


--
-- Lists are Monoids
--

-- We wrote 'instance Monoid [a]' and not 'instance Monoid []',
-- because Monoid requires a concrete type for an instance.
instance Monoid [a] where
    mempty = []
    mappend = (++)
-- 1st Monoid Law: mempty `mappend` [1,2,3] --> [1,2,3]
-- 2nd Monoid Law: [1,2,3] `mappend` mempty --> [1,2,3]
-- 3rd Monoid Law: ([1,2,3] `mappend` [4,5,6]) `mappend` [7,8,9] --> [1,2,3,4,5,6,7,8,9]
-- 3rd Monoid Law: [1,2,3] `mappend` ([4,5,6] `mappend` [7,8,9]) --> [1,2,3,4,5,6,7,8,9]


--
-- Numbers are Monoids (via Sum)
--

-- The Sum type from Data.Monoid
newtype Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

-- The Sum type is a Monoid
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)
-- 1st Monoid Law: getSum $ mempty `mappend` Sum 5 --> 5
-- 2nd Monoid Law: getSum $ Sum 5 `mappend` mempty --> 5
-- 3rd Monoid Law: getSum $ (Sum 5 `mappend` Sum 6) `mappend` Sum 10 --> 21
-- 3rd Monoid Law: getSum $ Sum 5 `mappend` (Sum 6 `mappend` Sum 10) --> 21


--
-- Numbers are Monoids (via Product)
--

-- The Product type from Data.Monoid
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

-- The Product type is a Monoid
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
-- 1st Monoid Law: getProduct $ mempty `mappend` Product 5 --> 5
-- 2nd Monoid Law: getProduct $ Product 5 `mappend` mempty --> 5
-- 3rd Monoid Law: getProduct $ (Product 5 `mappend` Product 6) `mappend` Product 10 --> 300
-- 3rd Monoid Law: getProduct $ Product 5 `mappend` (Product 6 `mappend` Product 10) --> 300


--
-- Bools are Monoids (via logical or)
--

-- The Any newtype constructor
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

-- The Any newtype contructor is a Monoid
instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)
-- 1st Monoid Law: getAny $ mempty `mappend` Any True --> True
-- 2nd Monoid Law: getAny $ Any True `mappend` mempty --> True
-- 3rd Monoid Law: getAny $ (Any True `mappend` Any True) `mappend` Any False --> True
-- 3rd Monoid Law: getAny $ Any True `mappend` (Any True `mappend` Any False) --> True


--
-- Bools are Monoids (via logical and)
--

-- The All newtype constructor
newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

-- The All newtype contructor is a Monoid
instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)
-- 1st Monoid Law: getAll $ mempty `mappend` All False --> False
-- 2nd Monoid Law: getAll $ All False `mappend` mempty --> False
-- 3rd Monoid Law: getAll $ (All True `mappend` All True) `mappend` All False --> False
-- 3rd Monoid Law: getAll $ All True `mappend` (All True `mappend` All False) --> False


--
-- Ordering is a Monoid
--

instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
-- 1st Monoid Law: mempty `mappend` GT --> GT
-- 2nd Monoid Law: GT `mappend` mempty --> GT
-- 3rd Monoid Law: (LT `mappend` EQ) `mappend` GT --> LT
-- 3rd Monoid Law: LT `mappend` (EQ `mappend` GT) --> LT

--
-- How is the Ordering Monoid useful?
-- Taking advantage of how Ordering is a Monoid implementing lengthCompare
--

-- No MONOID
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let lengthComparison = length x `compare` length y
                        alphaComparison = x `compare` y
                    in  if lengthComparison == EQ then alphaComparison else lengthComparison
-- lengthCompare "zen" "ants" --> LT
-- lengthCompare "zen" "ant"  --> GT

-- Using the fact that Ordering is a Monoid
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = let lengthComparison = length x `compare` length y
                         alphaComparison = x `compare` y
                     in  lengthComparison `mappend` alphaComparison
-- lengthCompare' "zen" "ants" --> LT
-- lengthCompare' "zen" "ant"  --> GT


--
-- Maybe a as Monoid only if its type parameter a is a Monoid as well
-- USEFULNESS: When dealing with Monoids as results of computations that may have failed,
--             (we can continue to treat them as normal Monoids)
--

instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
-- 1st Monoid Law: mempty `mappend` Just (Sum 5)                                  --> Just (Sum {getSum = 5})
-- 2nd Monoid Law: Just (Sum 5) `mappend` mempty                                  --> Just (Sum {getSum = 5})
-- 3rd Monoid Law: (Just (Sum 5) `mappend` Just (Sum 10)) `mappend` Just (Sum 20) --> Just (Sum {getSum = 35})
-- 3rd Monoid Law: Just (Sum 5) `mappend` (Just (Sum 10) `mappend` Just (Sum 20)) --> Just (Sum {getSum = 35})


--
-- Maybe a is a Monoid (using First a type)
-- USEFULNESS: When we have a bunch of Maybe values
--             and want to know if any of them (the leftmost) is a Just
--

-- First: Take a Maybe a and wrap it with a newtype (from Data.Monoid)
--        Return the leftmost non-Nothing value
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Read, Ord, Show)

-- The Maybe Monoid
instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x
-- 1st Monoid Law: getFirst $ mempty `mappend` First (Just 'a') --> Just 'a'
-- 2nd Monoid Law: getFirst $ First (Just 'a') `mappend` mempty --> Just 'a'
-- 3rd Monoid Law: getFirst $ (First Nothing `mappend` First (Just 'b')) `mappend` First (Just 'c') --> Just 'b'
-- 3rd Monoid Law: getFirst $ First Nothing `mappend` (First (Just 'b') `mappend` First (Just 'c')) --> Just 'b'

-- Usefulness of mconcat: getFirst . mconcat . map First $ [Nothing, Just 3, Nothing, Just 7] --> Just 3


--
-- Maybe a is a Monoid (using Last a type)
-- USEFULNESS: When we have a bunch of Maybe values
--             and want to know if any of them (the rightmost) is a Just
--

-- Last: Take a Maybe a and wrap it with a newtype (from Data.Monoid)
--        Return the rightmost non-Nothing value
newtype Last a = Last { getLast :: Maybe a }
    deriving (Eq, Read, Ord, Show)

-- The Maybe Monoid
instance Monoid (Last a) where
    mempty = Last Nothing
    _ `mappend` Last (Just x) = Last (Just x)
    x `mappend` Last Nothing = x
-- 1st Monoid Law: getLast $ mempty `mappend` Last (Just 'a') --> Just 'a'
-- 2nd Monoid Law: getLast $ Last (Just 'a') `mappend` mempty --> Just 'a'
-- 3rd Monoid Law: getLast $ (Last Nothing `mappend` Last (Just 'b')) `mappend` Last (Just 'c') --> Just 'c'
-- 3rd Monoid Law: getLast $ Last Nothing `mappend` (Last (Just 'b') `mappend` Last (Just 'c')) --> Just 'c'

-- Usefulness of mconcat: getLast . mconcat . map Last $ [Nothing, Just 3, Nothing, Just 7, Nothing] --> Just 7

