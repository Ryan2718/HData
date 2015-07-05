{-|
Description : Tests of the Data Structures
Copyright   : (c) Ryan Forsyth, 2015
-}
module Main (main) where

import Test.HUnit
import qualified Stack as S
import qualified Queue as Q

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

main :: IO ()
main = do
  runTestTT stackTests
  runTestTT queueTests
  return ()

