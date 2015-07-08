{-|
Description : Tests of the Data Structures
Copyright   : (c) Ryan Forsyth, 2015
-}
module Main (main) where

import Test.HUnit
import qualified Stack as S
import qualified Queue as Q
import qualified Tree as T

import Data.Foldable
import Prelude hiding (foldl, foldr)

stackTests :: Test
stackTests = test [
    "TestPush" ~: "" ~: S.Stack [1] ~=? (S.push S.empty 1)
  , "TestPop" ~: "" ~: S.empty ~=? (S.pop $ S.push S.empty 1)
  , "TestPeek" ~: "" ~: Just 4 ~=? (S.peek $ S.push S.empty 4)
  , "TestEmpty" ~: "" ~: Nothing ~=? (S.peek $ S.pop $ S.push S.empty 17)
  ]


q1 :: Q.Queue Int
q1 = foldl Q.enqueue Q.empty [1, 2, 3]

q2 :: Q.Queue Int
q2 = Q.dequeue q1

q3 :: Q.Queue Int
q3 =  Q.dequeue $ Q.dequeue q2

q4 :: Q.Queue Int
q4 = Q.dequeue (Q.enqueue (Q.enqueue q3 4) 5)

queueTests :: Test
queueTests = test [
    "testQ1" ~: "" ~: [1, 2, 3] ~=? Q.list q1
  , "testQ2" ~: "" ~: [2, 3] ~=? Q.list q2
  , "testQ3" ~: "" ~: [] ~=? Q.list q3
  , "testQ4" ~: "" ~: [5] ~=? Q.list q4
  , "testP1" ~: "" ~: Just 1 ~=? Q.peek q1
  , "testP2" ~: "" ~: Just 2 ~=? Q.peek q2
  , "testP3" ~: "" ~: Nothing ~=? Q.peek q3
  , "testP4" ~: "" ~: Just 5 ~=? Q.peek q4
  ]

t1 :: T.Tree Int
t1 = foldl T.insert T.None [4, 2, 6, 3, 7, 5, 1]

treeTests :: Test
treeTests = test [
      "testFmap" ~: "" ~: map (3*) [1,2,3,4,5,6,7] ~=?
                          (T.inOrder $ fmap (3*) t1)
    , "testFoldr" ~: "" ~: 28 ~=? foldr (+) 0 t1
    , "testPreOrder" ~: "" ~: [4, 2, 1, 3, 6, 5, 7] ~=? T.preOrder t1
    , "testInOrder" ~: "" ~: [1, 2, 3, 4, 5, 6, 7] ~=? T.inOrder t1
    , "testPostOrder" ~: "" ~: [1, 3, 2, 5, 7, 6, 4] ~=? T.postOrder t1
    , "testContainsTrue" ~: "" ~: True ~=? T.contains t1 5
    , "testContainsFalse" ~: "" ~: False ~=? T.contains t1 0
    ]

main :: IO ()
main = do
  runTestTT stackTests
  runTestTT queueTests
  runTestTT treeTests
  return ()

