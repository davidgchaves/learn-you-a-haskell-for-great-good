import Data.Monoid

--
-- DiffList: wrapper for difference lists so that
--           we can easily give them monoid instances.
--           The wrapped type is [a] -> [a], because a difference list is
--           a function that takes a list and returns another list.
--
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

--
-- toDiffList: Converts a normal list into a difference list
--
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

--
-- fromDiffList: Converts a difference list intoa a normal list
--
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

--
-- The Monoid instance for difference lists:
--  - mempty  --> is the id function
--  - mappend --> is the function composition
--
instance Monoid (DiffList a) where
    mempty                              = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
-- fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [10,20,30]) --> [1,2,3,4,10,20,30]

