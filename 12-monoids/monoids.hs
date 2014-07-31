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

