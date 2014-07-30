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
