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

