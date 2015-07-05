{-|
Description : Queue Abstract Data Type
Copyright   : (c) Ryan Forsyth, 2015
-}
module Queue
       (
         Queue()
         
         -- * Initialization
       , empty

          -- * Insertion
       , enqueue

         -- * Removal
       , dequeue

         -- * Observation
       , peek
       , list
       ) where

import qualified Stack as S


-- Queue implemented with two stacks.

-- Stack alpha is the beginning of the queue.
-- Items are enqueued by being placed on top of stack alpha.

-- Stack beta is the end of the queue.
-- Items are dequeued by being taken off the top of stack beta.

-- So, the element on the bottom of Stack alpha was enqueued right
-- after the element on the bottom of Stack beta.

-- | Queue Data Type
data Queue a = Queue {alpha :: S.Stack a, beta :: S.Stack a}

-- | Empty Queue
empty :: Queue a
empty = Queue {alpha = S.empty, beta = S.empty}

-- | Enqueue an element
enqueue :: Queue a -> a -> Queue a
enqueue (Queue a b) e = Queue (S.push a e) b

-- | Dequeue an element
dequeue :: Queue a -> Queue a
dequeue q@(Queue a b) =
  if (null $ S.list a) && (null $ S.list b)
  then q
  else case b of
   S.Stack (x:xs) ->  Queue a (S.Stack xs)
   S.Stack [] -> dequeue $ switch q


switch :: Queue a -> Queue a
switch (Queue a b) = Queue (S.Stack $ reverse $ S.list b)
                   (S.Stack $ reverse $ S.list a)

-- | Peek at the next element to be dequeued
peek :: Queue a -> Maybe a
peek q@(Queue a b) =
  if (null $ S.list a) && (null $ S.list b)
  then Nothing
  else case b of
   S.Stack (x:xs) -> Just x
   S.Stack [] -> let a' = S.list a
                     n = length a'
                 in Just $ a' !! (n - 1) 

-- | Turn the queue into a list
list :: Queue a -> [a]
list (Queue a b) = (S.list b) ++ (reverse $ S.list a) 
