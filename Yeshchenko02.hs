{-# OPTIONS_GHC -Wall #-}
module Yeshchenko02 where

-- Задача 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr [] = 0
sumFr (x:xs) = foldr (+) x xs
  
-- Задача 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n | n > 0 = foldl (*) 1 [1.. n]
            | otherwise = error "Argument n must be > 0."

-- Задача 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr [] [] = []
concatFr xs [] = xs
concatFr [] ys = ys
concatFr (x:xs) (y:ys) = foldr (:) (y:ys) (x:xs)

-- Задача 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert [] = []
sortInsert (x:xs) = foldl (insertx) [] (x:xs)

--aux func for inserting int it its place in the array
insertx :: [Integer] -> Integer -> [Integer]
insertx xs v = if (null xs) then v:xs
              else if (v - head xs) <= 0 then v : xs
              else head xs : insertx (tail xs) v

-- Задача 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ [] [] = []
map2 _ _ [] = []
map2 _ [] _ = []
map2 f xs ys = (f (head xs) (head ys)) : (map2 f (tail xs) (tail ys))

-- Задача 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n | m >= 0 = if n > 0 then ((fromIntegral) m^n / (fromIntegral) (factorial n)) + (expPart m (n-1))
                       else 0
            | otherwise = error "Argument m must be >= 0."

-- Задача 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [n | n <- [1 ..]]

-- Задача 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) [n*n | n <- [1 ..]]

-- Задача 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes [] [] = [0] -- just to meet task requirements; empty list otherwise.
indexes [] ys = [0..(length ys)] --just to meet task requirements: indexes [] [1] = [0, 1].
indexes _ [] = []
indexes xs ys = findSubLists xs ys 0

findSubLists :: [Int] -> [Int] -> Int -> [Int]
--finding and returning the indices list where the found substrings begin
--(the Int is current position in the list, where we are looking for sublists)
findSubLists [] [] _ = []
findSubLists [] ys n = n : findSubLists [] (tail ys) (n+1)
findSubLists _ [] _ = []
findSubLists xs ys n = if (length xs) == (checkCurrPos xs ys) then
                       n : findSubLists xs (tail ys) (n+1)
                       else findSubLists xs (tail ys) (n+1)

--aux func for checking whether there is a match in substrings from current i of ys; returns the length of matching sequence
checkCurrPos :: [Int] -> [Int] -> Int
checkCurrPos [] [] = 0
checkCurrPos [] _ = 0
checkCurrPos _ [] = 0
checkCurrPos xs ys = if (head xs) == (head ys) then
                     1 + checkCurrPos (tail xs) (tail ys)
                     else 0



--check if xs.length == res of function which checks whether the current element string has all the elements equal to the needed ones; if yes - add 1+ check tail of the ys

