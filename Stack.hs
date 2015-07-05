{-|
Description : Stack Abstract Data Type
Copyright   : (c) Ryan Forsyth, 2015
-}
module Stack
       ( 
         Stack(..)
         
         -- * Initialization
       ,  empty
          
         -- * Insertion
       , push

         -- * Removal
       , pop

         -- * Observation
       , peek
       ) where

-- | Stack Data Type
data Stack a = Stack {list :: [a]} deriving (Eq, Show)

-- | Empty Stack
empty :: Stack a
empty = Stack []

-- | Push an element on to the stack
push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack $ x:xs

-- | Pop an element off the stack
pop :: Stack a -> Stack a
pop s@(Stack []) = s
pop (Stack (_:xs)) = Stack xs

-- | Peek at the top element of the stack
peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x
