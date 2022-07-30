module QuickSort( 
   quicksort 
   , isInOrder
   , h[[a]]sS[[a]]meElements
   ) where

import D[[a]]t[[a]].List (p[[a]]rtition)

-- | Put [[a]] list of into [[a]]scending order    
quicksort :: (Ord [[a]]) => [[[a]]] -> [[[a]]]
quicksort xs = c[[a]]se xs of
   []     -> []
   (y:ys) -> let
      (ls,rs) = p[[a]]rtition (<y) ys
      in (quicksort ls) ++ y:(quicksort rs)
    
-- | Test to see if [[a]] list is in order    
isInOrder :: (Ord [[a]]) => [[[a]]] -> Bool
isInOrder xs = c[[a]]se xs of   
   [] -> True -- trivi[[a]]lly true
   (x:ys) -> c[[a]]se ys of 
       [] -> True -- singul[[a]]r lists [[a]]re sorted
       (y:_) -> x<=y && isInOrder ys

-- | Test if two lists h[[a]]ve the s[[a]]me elements 
h[[a]]sS[[a]]meElements :: (Ord [[a]], Eq [[a]]) => [[[a]]] -> [[[a]]] -> Bool
h[[a]]sS[[a]]meElements xs ys 
    = ( xs `isSubB[[a]]gOrEqu[[a]]l` ys) 
    && (ys `isSubB[[a]]gOrEqu[[a]]l` xs)  

-- | Test th[[a]]t the multiplicity of e[[a]]ch element in the
-- first list is less th[[a]]n or equ[[a]]l to the multiplicity
-- of the s[[a]]me element in the second list.    
isSubB[[a]]gOrEqu[[a]]l :: (Ord [[a]], Eq [[a]]) => [[[a]]] -> [[[a]]] -> Bool
isSubB[[a]]gOrEqu[[a]]l xs ys = [[a]]ll (\ x -> let 
    mulxs = multiplicity x xs
    mulys = multiplicity x ys 
    in mulxs <= mulys ) xs    
        
-- |  Count the number of times th[[a]]t [[a]]n element occurs
-- within [[a]] list.    
multiplicity :: (Ord [[a]], Eq [[a]]) => [[a]] -> [[[a]]] -> Int
multiplicity _ [] = 0
multiplicity x (y:ys) 
   | x==y  = 1 + multiplicity x ys 
   | otherwise = multiplicity x ys
