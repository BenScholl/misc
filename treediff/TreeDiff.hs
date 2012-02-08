module TreeDiff
    ( treeDiff
    , Cost (..)
    , Tree (..)
    , EditAction (..)
    ) where

import Data.Monoid
import qualified Data.MemoCombinators as Memo

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data Tree a = Tree a [Tree a]
        deriving (Show, Eq)

data EditAction = Same Int Int | Update Int Int | Delete Int | Insert Int
        deriving (Show, Eq)

-- Represents the cost of a route of the edit graph, along with the actions taken on that route
-- 'Infinity' represents paths that can't be taken, as in the original paper
data Cost = Cost Int [EditAction] | Infinity
        deriving (Show, Eq)

instance Ord Cost where
        Infinity `compare` Infinity = EQ
        Infinity `compare` a = GT
        a `compare` Infinity = LT
        Cost a _ `compare` Cost b _ = a `compare` b

instance Monoid Cost where
        mempty = Cost 0 mempty
        Infinity `mappend` a = Infinity
        a `mappend` Infinity = Infinity
        Cost a as `mappend` Cost b bs = Cost (a+b) (as `mappend` bs)

data DepthInfo a = DepthInfo { nodeValue :: a, depth :: Int }

buildDepthInfo :: Int -> Tree a -> [DepthInfo a]
buildDepthInfo n (Tree value branches) = DepthInfo value n : concatMap (buildDepthInfo (n+1)) branches

treeDiff :: Eq a => Tree a -> Tree a -> Cost
treeDiff treeA treeB = editGraphDistance (itemsA, itemCountA) (itemsB, itemCountB) itemCountA itemCountB
    where
        itemsA = reverse $ buildDepthInfo 0 treeA
        itemsB = reverse $ buildDepthInfo 0 treeB
        itemCountA = length itemsA
        itemCountB = length itemsB

-- The DepthInfo lists passed in need to be in reverse order.
-- This is because we begin at the furthest point of the edit graph, and work back to the start.
editGraphDistance :: Eq a => ([DepthInfo a], Int) -> ([DepthInfo a], Int) -> Int -> Int -> Cost
editGraphDistance a@(itemsA, itemCountA) b@(itemsB, itemCountB) = distanceMemo where
  distanceMemo = (Memo.memo2 Memo.bits Memo.bits) distance
  distance indexA indexB
    | indexA == 0 && indexB == 0  = mempty
    | indexA == 0                 = recurse 0 (indexB-1) <> costInsert indexB
    | indexB == 0                 = recurse (indexA-1) 0 <> costDelete indexA
    | otherwise                   = minimum [m1,m2,m3]
    where
        currentItemA = itemsA !! (itemCountA - indexA)
        currentItemB = itemsB !! (itemCountB - indexB)
        
        m1 = if depth currentItemA == depth currentItemB    then update else Infinity
        m2 = if depthLessThan b indexB (depth currentItemA) then delete else Infinity
        m3 = if depthLessThan a indexA (depth currentItemB) then insert else Infinity

        update = recurse (indexA - 1) (indexB - 1) <> costUpdate currentItemA indexA currentItemB indexB
        delete = recurse (indexA - 1)  indexB      <> costDelete indexA
        insert = recurse  indexA      (indexB - 1) <> costInsert indexB
        recurse = distanceMemo

depthLessThan :: ([DepthInfo a], Int) -> Int -> Int -> Bool
depthLessThan (items, itemCount) index d =
      itemCount == index
   || depth (items !! (itemCount - index - 1)) <= d

costDelete :: Int -> Cost
costDelete i = Cost 1 [Delete i]

costInsert :: Int -> Cost
costInsert i = Cost 1 [Insert i]

costUpdate :: Eq a => DepthInfo a -> Int -> DepthInfo a -> Int -> Cost
costUpdate a aLen b bLen = if nodeValue a == nodeValue b
        then Cost 0 [Same aLen bLen]
        else Cost 1 [Update aLen bLen]
