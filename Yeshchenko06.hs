{-# OPTIONS_GHC -Wall #-}
module Yeshchenko06 where

--import Data.Tree
--import Data.List.Sort

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Ord, Show)

-- Задача 1 -----------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) | x == y = isPrefix xs ys
                       | otherwise = False

-- Задача 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] ys = ([], [], ys)
partition xs [] = ([], xs, [])
--partition (x:xs) (y:ys) | x == y = ([x] [] []) : partition xs ys
--                        | otherwise = xs ys
partition xs ys = ((partitionAux xs ys), (drop ((length (partitionAux xs ys))) xs), (drop ((length (partitionAux xs ys))) ys))

-- aux func, where the the res is the same prefix of both strings
partitionAux :: Eq a => [a] -> [a] -> [a]
partitionAux _ [] = []
partitionAux [] _ = []
partitionAux xs ys | (head xs) == (head ys) = head xs : partitionAux (tail xs) (tail ys)
                   | otherwise = []

-- partition2 :: Eq a => [a] -> [a] -> ([a], [a], [a])
-- partition2 [] ys = ([], [], ys)
-- partition2 xs [] = ([], xs, [])
-- partition2 (x:xs) (y:ys) | x == y = ([x] [] []) : partition2 xs ys
                         -- | otherwise = ([] (x:xs) (y:ys))
-- Задача 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s = s : suffixes (tail s)

-- Задача 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring [] _ = True
isSubstring _ [] = False
isSubstring xs ys | isPrefix xs ys = True
                  | otherwise = isSubstring xs (tail ys)

-- Задача 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
--findSubstrings :: [Int] -> [Int] -> [Int]
--findSubstrings [] [] = [] -- just to meet task requirements; empty list otherwise.
findSubstrings [] _ = [] --just to meet task requirements: indexes [] [1] = [0, 1].
findSubstrings _ [] = []
findSubstrings xs ys = findSubLists xs ys 0

findSubLists :: String -> String -> Int -> [Int]
--finding and returning the indices list where the found substrings begin
--(the Int is current position in the list, where we are looking for sublists)
findSubLists [] [] _ = []
findSubLists [] ys n = n : findSubLists [] (tail ys) (n+1)
findSubLists _ [] _ = []
findSubLists xs ys n = if (length xs) == (checkCurrPos xs ys) then
                       n : findSubLists xs (tail ys) (n+1)
                       else findSubLists xs (tail ys) (n+1)

--aux func for checking whether there is a match in substrings from current i of ys; returns the length of matching sequence
checkCurrPos :: String -> String -> Int
checkCurrPos [] [] = 0
checkCurrPos [] _ = 0
checkCurrPos _ [] = 0
checkCurrPos xs ys = if (head xs) == (head ys) then
                     1 + checkCurrPos (tail xs) (tail ys)
                     else 0

-- findSubstrings2 :: String -> String -> [Int]
-- findSubstrings2 xs ys =
-- Задача 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf l) = [l]
getIndices (Node []) = []
getIndices (Node ((_, st):[])) = sortInsert (getIndices st)
getIndices (Node ((_, st):xs)) = sortInsert ((getIndices st) ++ (getIndices (Node ((head xs) :(tail xs)))))
--getIndices (Node [(_, st)]):xs = [0]
--getIndices (Node (_ x)) = [l]
--getIndices (Node (x:xs)) = []
--getIndices (Node [((Leaf l), st)]) = [l]
--getIndices (Node ((Leaf l) : xs)) = getIndices l ++ getIndices xs -- (getIndices x) ++ (getIndices xs)

--getIndices (Node [(_, st)]) = getIndices st

-- aux sort function
sortInsert :: [Int] -> [Int]
sortInsert [] = []
sortInsert (x:xs) = foldl (insertx) [] (x:xs)

-- aux func for inserting int it its place in the array
insertx :: [Int] -> Int -> [Int]
insertx xs v = if (null xs) then v:xs
              else if (v - head xs) <= 0 then v : xs
              else head xs : insertx (tail xs) v
-- Задача 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf l) = [l]
findSubstrings' _ (Node []) = []
--   findSubstrings' _ (Node []) = (getIndices (Node []))
--findSubstrings' [] (Node ((_, st):[])) =  (getIndices st)
--findSubstrings' [] (Node ((_, st):xs)) =  (getIndices st) ++ (findSubstrings' [] (Node [(head xs)]))
--       findSubstrings' str (Node ((s, (Leaf l)) = [l]
-- findSubstrings' str (Node ((s, st):[])) | (isPrefix str s) =  (getIndices st)
                                        -- | (isPrefix s str) = (findSubstrings' (drop ((length (partitionAux s str))) str) st)
                                        -- | otherwise = (findSubstrings' str (Node [])) -- = []
findSubstrings' str (Node ((s, st):xs)) | (isPrefix str s) = (getIndices st) -- ++ (findSubstrings' str (Node [(head xs)]))
                                        | (isPrefix s str) = (findSubstrings' (drop ((length (partitionAux s str))) str) st) -- ++ (findSubstrings' str (Node [(head xs)]))
                                        | otherwise = (findSubstrings' str (Node xs)) -- (Node ((head xs) :(tail xs)))

-- Задача 8 -----------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
-- insert = undefined
insert (_,_) (Leaf _) = Node []
insert (s, n) (Node []) = Node [(s, (Leaf n))]

--insert (snew, n) (Node ((s, st):[])) = undefined -- repeat the same as for non-empty, but: otherwise = (Node ((s, st):[Node snew [Leaf n]]))
insert (snew, n) (Node ((s, st):xs)) | ((length (partitionAux snew s)) == 0) = insert (snew, n) (Node xs)
                                     | ((length (partitionAux snew s)) > 0) && (isPrefix snew (partitionAux snew s)) = Node ((s, insert ((drop (length (partitionAux snew s)) snew), n) st):xs) --insert ((drop (length (partitionAux snew s)) snew), n) st
                                     | ((length (partitionAux snew s)) > 0) = Node [((partitionAux snew s), Node [((drop (length (partitionAux snew s)) snew), (Leaf n)), ((drop (length (partitionAux snew s)) s), st)])]
                                     | otherwise = (Node [(s, Node[(snew, (Leaf n))])]) --insert (snew, n) (Node xs) !!! -- = Node [(s, Node[(snew, (Leaf n)) : st])]

-- getContent (Node listOfPairs)
  -- = listOfPairs

-- insert :: (String, Int) -> SuffixTree -> SuffixTree
-- insert (s, n) (Node [])
  -- = Node [(s, Leaf n)]
-- insert (s, n) (Node (pair@(a, tree) : pairs))
  -- | null p   = Node (pair : (getContent (insert (r, n) (Node pairs))))
  -- | p == a   = Node ((a, insert (r, n) tree) : pairs)
  -- | p /= a   = Node ([(p, Node [(r, Leaf n), (r', tree)])] ++ pairs)
  -- where
    -- (p, r, r') = partition s a

-- Ця функція задана
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

-- Задача 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring = undefined


------------------------------------------------------
-- Приклади рядків і суфіксних дерев..

s1 :: String
s1 = "banana"

s2 :: String
s2  = "mississippi"

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]