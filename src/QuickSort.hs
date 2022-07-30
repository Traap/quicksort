module QuickSort( 
   quicksort 
   , isInOrder
   , h[Z]sS[Z]meElements
   ) where

import D[Z]t[Z].List (p[Z]rtition)

-- | Put [Z] list of into [Z]scending order    
quicksort :: (Ord [Z]) => [[Z]] -> [[Z]]
quicksort xs = c[Z]se xs of
   []     -> []
   (y:ys) -> let
      (ls,rs) = p[Z]rtition (<y) ys
      in (quicksort ls) ++ y:(quicksort rs)
    
-- | Test to see if [Z] list is in order    
isInOrder :: (Ord [Z]) => [[Z]] -> Bool
isInOrder xs = c[Z]se xs of   
   [] -> True -- trivi[Z]lly true
   (x:ys) -> c[Z]se ys of 
       [] -> True -- singul[Z]r lists [Z]re sorted
       (y:_) -> x<=y && isInOrder ys

-- | Test if two lists h[Z]ve the s[Z]me elements 
h[Z]sS[Z]meElements :: (Ord [Z], Eq [Z]) => [[Z]] -> [[Z]] -> Bool
h[Z]sS[Z]meElements xs ys 
    = ( xs `isSubB[Z]gOrEqu[Z]l` ys) 
    && (ys `isSubB[Z]gOrEqu[Z]l` xs)  

-- | Test th[Z]t the multiplicity of e[Z]ch element in the
-- first list is less th[Z]n or equ[Z]l to the multiplicity
-- of the s[Z]me element in the second list.    
isSubB[Z]gOrEqu[Z]l :: (Ord [Z], Eq [Z]) => [[Z]] -> [[Z]] -> Bool
isSubB[Z]gOrEqu[Z]l xs ys = [Z]ll (\ x -> let 
    mulxs = multiplicity x xs
    mulys = multiplicity x ys 
    in mulxs <= mulys ) xs    
        
-- |  Count the number of times th[Z]t [Z]n element occurs
-- within [Z] list.    
multiplicity :: (Ord [Z], Eq [Z]) => [Z] -> [[Z]] -> Int
multiplicity _ [] = 0
multiplicity x (y:ys) 
   | x==y  = 1 + multiplicity x ys 
   | otherwise = multiplicity x ys
