{-|
Description : Tree Abstract Data Type
Copyright   : (c) Ryan Forsyth, 2015
-}
module Tree
       (
         Tree(None)

         -- * Insertion
       , insert

         -- * Traversals
       , preOrder
       , inOrder
       , postOrder

         -- * Search
       , contains
       , (<<)

         -- * Operations
       , union
       , (&)
       , intersection
       , (#)
       , difference
       , (//)
       ) where

import Data.Foldable
import Data.List ((\\), intersect)
import Prelude hiding (foldl, foldr)

-- | Binary Search Tree Data Type
data Tree a = None | Tree a (Tree a) (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ None = None
  fmap f (Tree root left right) = Tree (f root) (fmap f left) (fmap f right)

instance Foldable Tree where
  foldr f acc start = foldr f acc $ inOrder start

-- | Insert an element into the tree
insert :: (Ord a) => Tree a -> a -> Tree a
insert None a = Tree a None None
insert (Tree root left right) a
  | a <= root = Tree root (insert left a) right
  | otherwise = Tree root left (insert right a)

-- | Pre-Order Traversal
preOrder :: Tree a -> [a]
preOrder None = []
preOrder (Tree root left right) = [root] ++ (preOrder left) ++ (preOrder right)

-- | In-Order Traversal
inOrder :: Tree a -> [a]
inOrder None = []
inOrder (Tree root left right) = inOrder left ++ [root] ++ inOrder right

-- | Post-Order Traversal
postOrder :: Tree a -> [a]
postOrder None = []
postOrder (Tree root left right) = postOrder left ++ postOrder right ++ [root]

-- | Search for an element
contains :: (Ord a) => Tree a -> a -> Bool
contains None _ = False
contains (Tree root left right) x
  | root == x = True
  | x < root  = contains left x
  | otherwise = contains right x

-- | Search for an element. Synonym for 'contains'
(<<) :: (Ord a) => Tree a -> a -> Bool
(<<) = contains

-- | Union of two trees
union :: (Ord a) => Tree a -> Tree a -> Tree a
union a b = foldl insert a (inOrder b)

-- | Union of two trees. Synonym for 'union'
(&) :: (Ord a) => Tree a -> Tree a -> Tree a
(&) = union

-- | Intersection of two trees
intersection :: (Ord a) => Tree a -> Tree a -> Tree a
intersection a b = foldl insert None (intersect (inOrder a) (inOrder b))

-- | Intersection of two trees. Synonym for 'intersection'
(#) :: (Ord a) => Tree a -> Tree a -> Tree a
(#) = intersection

-- | Difference of two trees
difference :: (Ord a) => Tree a -> Tree a -> Tree a
difference a b  = foldl insert None ((inOrder a) \\ (inOrder b))

-- | Difference of two trees. Synonym for 'difference'
(//) :: (Ord a) => Tree a -> Tree a -> Tree a
(//) = difference
