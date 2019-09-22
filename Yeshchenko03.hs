{-# OPTIONS_GHC -Wall #-}
module Yeshchenko03 where

-- Mastermind -----------------------------------------

-- Фішка може мати один з шести кольорів
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- Код - просто список фішок
type Code = [Peg]

-- Крок гри (Move) будує конструктор Move використовуючи код (Code) і два цілих;  
-- кількість повних і часткових відповідностей кода-пропозиції і шифру
data Move = Move Code Int Int deriving (Show, Eq)

-- Список містить всі різні кольори
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
--exactMatches [] [] = 0
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:cd) (y:pr) | (x == y) = 1 + (exactMatches cd pr)
                             | otherwise = (exactMatches cd pr)
-- Задача 2 -----------------------------------------
countColors :: Code -> [Int]
countColors cd = [(exactMatches cd [x, x, x, x]) | x <- colors]

--factorialsM = [factorial x | x <- [1 ..]]
--borders xs = [y | y <- suffixs xs, isPrefix y xs]

-- Задача 3 ----------------------------------------- 
matches :: Code -> Code -> Int
--matches [] [] = 0
matches [] _ = 0
matches _ [] = 0
--matches cd pr = sum [y | x <- countColors cd, y <- countColors pr, y > 0 && y <= x]
matches cd pr = sum (chooseXYTwo (countColors cd) (countColors pr))

--aux func for comparing two lists of the color occurences,
--returning a number of fully, as well as partial, guesses
chooseXYTwo :: [Int] -> [Int] -> [Int]
chooseXYTwo [] _ = []
chooseXYTwo _ [] = []
chooseXYTwo (x:xs) (y:ys) | (y > 0) && (x > 0) && (y <= x) = y : chooseXYTwo xs ys
                          | (y > 0) && (x > 0) && (y > x) = x : chooseXYTwo xs ys
                          | otherwise = 0 : chooseXYTwo xs ys
 
-- Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd pr = Move pr (exactMatches cd pr) (matches cd pr - exactMatches cd pr)

-- Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move pr f p) cd | (exactMatches cd pr) == f && ((matches cd pr) - (exactMatches cd pr)) == p = True
                   | otherwise = False

-- Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes _ [] = []
filterCodes mv (x:cs) | (isConsistent mv x) = x : filterCodes mv cs
                      | otherwise = filterCodes mv cs

-- Задача 7 -----------------------------------------
--allCodes :: Int -> [Code]
--allCodes 1 = [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
--allCodes n = concatMap longerCodes (allCodes (n-1))
--TBD      allCodes n = longerCodes (allCodes (n-1))

--aux func which takes Codes of (n-1) length and returns all codes of n length
--longerCodes :: Code -> [Code]
--longerCodes cs = undefined

--TBD      longerCodes cs = concatMap f cs

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
allCodes n = concatMap (\c -> map (c:) $ allCodes (n - 1)) colors

--tst
--justChecking :: Int -> [Int]
--justChecking n 
   
-- Задача 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = loop [initialMove]
  where codeLen = length cd
        initialMove = getMove cd $ replicate codeLen Red
        isConsistentWithAll ms c = all (`isConsistent` c) ms
        nextMove ms = getMove cd $ head $ filter (isConsistentWithAll ms) $ allCodes codeLen
        loop [] = []
        loop ms@(Move _ e _ : _)
          | e == codeLen = ms
          | otherwise    = loop (nextMove ms : ms)