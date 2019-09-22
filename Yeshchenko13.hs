{-# OPTIONS_GHC -Wall #-}
module Yeshchenko13 where


-- Задача 1 ------------------------------------------
lucky ::   Int ->  [String]
lucky n = [show num | num <- [(genLowerBound (2*n))..(genUpperBound (2*n))], checkIfLucky num]

--aux func, which checks whether first half of digits in 2n number equals the second half
checkIfLucky :: Int -> Bool
checkIfLucky n | (sum fHalf) == (sum sHalf) = True
               | otherwise = False
                 where digits = digs n
                       (fHalf, sHalf) = splitAt ((length digits) `div` 2) digits

--aux func for getting digits of a number
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

--aux func for generating the lower bound of a list (the min int of a given digit length)
--the argument is 2*n
genLowerBound :: Int -> Int
genLowerBound n = fromDigits (1 : (genLowerBoundAux (n-1)))

--arg is 2n-1
genLowerBoundAux :: Int -> [Int]
genLowerBoundAux 0 = []
genLowerBoundAux n = 0 : genLowerBoundAux (n-1)

--aux func for generating the upper bound of a list (the max int of a given digit length)
--the argument is 2*n
genUpperBound :: Int -> Int
genUpperBound n = fromDigits (genUpperBoundAux n)

--arg is 2n
genUpperBoundAux :: Int -> [Int]
genUpperBoundAux 0 = []
genUpperBoundAux n = 9 : genUpperBoundAux (n-1)

--aux func for concat list of ints into one int
fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

-- Задача 2 -----------------------------------------  
queens ::  Int -> [[Int]]
queens x = filter check (create x)
           where create 0 = [[]]
                 create m = [y : ys | y <- [1..x], ys <- create (m-1)]
                 check [] = True
                 check (y:ys) = isSafe y ys && check ys
                 isSafe   try ys = not (elem try ys || checkd try ys)
                 checkd try ys = any (\(numc,y) -> abs (try - y) == numc) $ zip [1..] ys
   
-- Задача 3 -----------------------------------------
maxLen ::  [Int] -> Int
maxLen xs = maxLenAux 0 (allLists xs)

--aux func which finds the longest list from an already built list of lists
maxLenAux :: Int -> [[Int]] -> Int
maxLenAux n [] = n
maxLenAux n (xs:xss) | (length xs) > n = maxLenAux (length xs) xss
                     | otherwise = maxLenAux n xss

--aux func for getting all the Int lists, elems in which are growing upwards
allLists :: [Int] -> [[Int]]
allLists [] = []
--allLists (x:xs) = (currList x (x:xs)) : allLists xs
allLists (x:xs) = [x:(currList x skips xs) | skips <- [0.. (length xs)]] ++ allLists xs

-- currList :: Int -> [Int] -> [Int]
-- currList _ [] = []
-- currList n (x:xs) | x > n = x : currList x xs
                  -- | otherwise = currList n xs

--aux func for getting upward growing list based on current first elem
--first arg is initial num, second arg is #of skips for the current go (we skip the given amount of elems, which are bigger than the initial)
currList :: Int -> Int -> [Int] -> [Int]
currList _ _ [] = []
currList n s (x:xs) | (x > n) && (s > 0) = currList n (s-1) xs
                    | (x > n) = x : currList x s xs
                    | otherwise = currList n s xs
   
-- Задача 4 -----------------------------------------
maxSeq ::  [Int] ->  [Int]
maxSeq xs = maxSeqAux maxL xss
         where xss = allLists xs
               maxL = maxLen xs

--aux func, which returns the first list with the length specified as the first argument
maxSeqAux :: Int -> [[Int]] -> [Int]
maxSeqAux _ [] = []
maxSeqAux n (xs:xss) | n == (length xs) = xs
                     | otherwise = maxSeqAux n xss

-- Задача 5 -----------------------------------------
allMaxSeq ::  [Int] -> [[Int]]
allMaxSeq xs = allMaxSeqAux maxL xss
               where xss = allLists xs
                     maxL = maxLen xs

allMaxSeqAux :: Int -> [[Int]] -> [[Int]]
allMaxSeqAux _ [] = []
allMaxSeqAux n (xs:xss) | n == (length xs) = xs : allMaxSeqAux n xss
                        | otherwise = allMaxSeqAux n xss
-- Задача 6 -----------------------------------------
genExpr ::  Int -> Int -> [String]
genExpr = undefined

-- Задача 7 -----------------------------------------
genExprBracket ::  Int -> Int -> [String]
genExprBracket = undefined

--tests for task 3
-- allLists [3,5,2,9,6,1,12] == [[3,5,9,12],[3,9,12],[3,6,12],[3,12],[3],[3],[3],[5,9,12],[5,6,12],[5,12],[5],[5],[5],[2,9,12],[2,6,12],[2,12],[2],[2],[9,12],[9],[9],[9],[6,12],[6],[6],[1,12],[1],[12]]
-- currList 3 0 [5,2,9,6,1,12] == [5,9,12]
