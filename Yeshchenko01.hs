{-# OPTIONS_GHC -Wall #-}
module Yeshchenko01 where

-- Задача 1 -----------------------------------------
factorial :: Integer -> Integer
factorial n = if n == 0 then 1 
              else n * (factorial (n-1))

-- Задача 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if (not (null xs)) && (not (null ys)) then
                (head xs) + (head ys) : (listSum (tail xs) (tail ys))
                else if (null xs) && (not (null ys)) then
                head ys : (listSum (xs) (tail ys))
                else if (null ys) && (not (null xs)) then
                head xs : (listSum (tail xs) (ys))
                else []

-- Задача 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven xs = if null xs then []
             else if (null (tail xs)) then
             head xs : oddEven (tail xs)
             else if (even (head xs)) && (even (head (tail xs))) || (odd (head xs)) && (odd (head (tail xs))) then
             head (tail xs) : head xs  : oddEven (tail (tail xs))
             else head xs : oddEven (tail xs)

-- Задача 4 -----------------------------------------
position    ::  Int -> [Int] -> Int
position n xs = if (null xs) || (notElem n xs) then -1
--did not check if it is the last element and it is not on the list!!!!!!!!
                --else if (null (tail xs)) && (head xs) /= n then
                ---1
                else if (head xs) == n then
                0
                else 1+(position n (tail xs))
                     
-- Задача 5 -----------------------------------------
set :: [Int] -> [Int] 
set xs = if null xs then []
         else (head xs) : set (removeDupes (head xs) (tail xs))

-- auxiliary function to remove duplicates from the list
removeDupes :: Int -> [Int] -> [Int]
removeDupes n xs = if null xs then []
                   else if (n - (head xs)) == 0 then
                   removeDupes n (tail xs)
                   else (head xs) : (removeDupes n (tail xs))
-- Задача 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set (mergeTwoLists (set (xs)) (set (ys)))

-- aux func mergeTwoLists
mergeTwoLists :: [Int] -> [Int] -> [Int]
mergeTwoLists xs ys = if null xs then ys else
                head xs : mergeTwoLists (tail xs) ys

-- Задача 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = setIntrs (mergeTwoLists (set (xs)) (set (ys)))

-- aux func setIntrs to leave elements which appear twice (were in both lists prior to the merge)
setIntrs :: [Int] -> [Int]
setIntrs xs = if null xs then []
                   else if checkPresence (head xs) (tail xs) then
                   --adding
                   head xs : setIntrs (tail xs)
                   else
                   --not adding
                   setIntrs (tail xs)

-- auxiliary function to check whether an element is already present in the final list
checkPresence :: Int -> [Int] -> Bool
checkPresence n xs = if null xs then False
                     else if (n-(head xs)) == 0 then
                     True
                     else checkPresence n (tail xs)
-- Задача 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1 ..]]
--factorialsM = 1 : zipWith (*) factorialsM [2..]