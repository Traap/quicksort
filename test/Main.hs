{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import QuickSort

testCount :: Int
testCount = 1000

main :: IO ()
main = defaultMain tests

-- A list of all the unit tests to perform
tests :: [Test]
tests = 
   [ testIsSorted, testHasSameElements ]
   
testIsSorted :: Test
testIsSorted = testP testCount "IsSorted" $
   forAll (listOf (choose (1,10::Int))) (\ xs -> isInOrder (quicksort xs))
   
testHasSameElements :: Test
testHasSameElements = testP testCount "HasSameElements" $
   forAll (listOf (choose (1,10::Int))) (\ xs -> hasSameElements xs (quicksort xs))


testP :: Int -> String -> Property -> Test
testP n name prop = plusTestOptions (mempty {topt_maximum_generated_tests = Just n,topt_maximum_unsuitable_generated_tests = Just (n * 4)}) (testProperty name prop)
