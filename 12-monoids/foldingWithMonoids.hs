import qualified Data.Foldable as F
import Data.Monoid

--
-- Foldable Trees
--

-- A binary search tree (Check Chapter 07)
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show)

-- Making Trees an instance of Foldable (implementing foldMap)
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

testTree = Node 5
            (Node 3
                (Node 1 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )
-- F.foldl (+) 0 testTree --> 42
-- F.foldl (*) 1 testTree --> 64800

-- Reducing testTree to a single Monoid value:
-- 1: Any number in testTree is 3?
--      getAny $ F.foldMap (\x -> Any $ x == 3) testTree --> True
-- 2: Any number in testTree is greater than 15?
--      getAny $ F.foldMap (\x -> Any $ x > 15) testTree --> False
-- 3: Convert testTree into a List
--      F.foldMap (\x -> [x]) testTree --> [1,3,6,5,8,9,10]

