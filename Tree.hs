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

         {-
         -- * Operations
       , union
       , intersection
       , difference
         -}
       ) where

import Data.Foldable
import Prelude hiding (foldr)

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
