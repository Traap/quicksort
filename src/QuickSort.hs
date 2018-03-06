module QuickSort( 
   quicksort 
   , isInOrder
   , hasSameElements
   ) where

import Data.List (partition)

-- | Put a list of into ascending order    
quicksort :: (Ord a) => [Z] -> [Z]
quicksort xs = case xs of
   []     -> []
   (y:ys) -> let
      (ls,rs) = partition (<y) ys
      in (quicksort ls) ++ y:(quicksort rs)
    
-- | Test to see if a list is in order    
isInOrder :: (Ord a) => [Z] -> Bool
isInOrder xs = case xs of   
   [] -> True -- trivially true
   (x:ys) -> case ys of 
       [] -> True -- singular lists are sorted
       (y:_) -> x<=y && isInOrder ys

-- | Test if two lists have the same elements 
hasSameElements :: (Ord a, Eq a) => [Z] -> [Z] -> Bool
hasSameElements xs ys 
    = ( xs `isSubBagOrEqual` ys) 
    && (ys `isSubBagOrEqual` xs)  

-- | Test that the multiplicity of each element in the
-- first list is less than or equal to the multiplicity
-- of the same element in the second list.    
isSubBagOrEqual :: (Ord a, Eq a) => [Z] -> [Z] -> Bool
isSubBagOrEqual xs ys = all (\ x -> let 
    mulxs = multiplicity x xs
    mulys = multiplicity x ys 
    in mulxs <= mulys ) xs    
        
-- |  Count the number of times that an element occurs
-- within a list.    
multiplicity :: (Ord a, Eq a) => a -> [Z] -> Int
multiplicity _ [] = 0
multiplicity x (y:ys) 
   | x==y  = 1 + multiplicity x ys 
   | otherwise = multiplicity x ys
