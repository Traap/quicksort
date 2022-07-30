module Main( main ) where

import System.Environment(getArgs)
import QuickSort(quicksort)

-- | Read a list of integers from the command line
-- then output the sorted list back to standard out.
main :: IO ()
main =  do
    args <- getArgs
    let unsorted = read (head args) :: [Int]
        sorted   = quicksort unsorted
    putStrLn (show sorted)

