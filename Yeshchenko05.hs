{-# OPTIONS_GHC -Wall #-}
module Yeshchenko05 where

import Data.Tree

-- Задача 1 -----------------------------------------			   
rank :: Tree a -> Int
rank t = countTreesInForest (getSons t)
-- rank t = length (getSons t) - !
-- aux func for getting list of sons
getSons :: Tree a -> Forest a
getSons (Node _ xs) = xs

-- aux func for counting number of trees in a given forest (omitting subtrees)
countTreesInForest :: Forest a -> Int
countTreesInForest [] = 0
countTreesInForest (_:xs) = 1 + countTreesInForest xs
-- далі треба було перевірити умову мінімальності у задачі 2! (кожний батько більший за кожного сина)
-- Задача 2-----------------------------------------
isBinomTree :: Ord a => Tree a -> Bool
isBinomTree (Node _ []) = True
isBinomTree tr = checkAllTrue ([((x:xs) == (z:zs))]) where (z:zs) = 0:[2^i | i <- [0..(rank tr)-1]]
                                                           (x:xs) = reverse (getLenList (createElLists (getSons tr)))
                                                    
-- 2^i == num of nodes in every next subtree

-- count number of elements in a node; then compare with expected amount - 2^i
--first, I get all subtrees via (getSons tr) and get a forest of trees from smallest to largest;
--afterwards, I check them one by one by comparing number of nodes there with the expected amount

-- aux func
dfsForest ::  Forest a -> [a]
dfsForest [] = []
dfsForest (Node x []:ts) = x : dfsForest ts
dfsForest (Node x xs:ts) = x : (dfsForest xs) ++ dfsForest ts

-- aux func - list all elements in a tree as a list
getAllNodes :: Tree a -> [a]
getAllNodes (Node x xs) = x : dfsForest xs

--aux func - create list of lists, where every list contains elements from one of the trees
createElLists :: Forest a -> [[a]]
createElLists [] = [[]]
createElLists (Node x xs:ts) = (x : (dfsForest xs)) : (createElLists ts)

--aux func for checking whether all nodes' lengths are equal to expected ones
checkAllTrue :: [Bool] -> Bool
checkAllTrue = foldr (&&) True

-- aux func for getting list of lengths
getLenList :: [[a]] -> [Int]
getLenList [] = []
getLenList (xs:xss) = (length xs) : (getLenList xss)
-- Задача 3 -----------------------------------------
isBinomHeap :: Ord a => Forest a -> Bool
isBinomHeap ts = checkAllTrue ([isBinomTree t | t <- ts])

-- Задача 4 -----------------------------------------
combineTrees :: Ord a => Tree a -> Tree a -> Tree a
combineTrees t ttwo | (rank t) == (rank ttwo) = combineAux t ttwo
                   | otherwise = error "Tree ranks are not equal, can not combine!"

combineAux :: Ord a => Tree a -> Tree a -> Tree a
combineAux (Node x xs) (Node y ys) | x < y = Node x ((Node y ys) : xs)
                                   | otherwise = Node y ((Node x xs) : ys)
-- Задача 5 -----------------------------------------
extractMin :: Ord a => Forest a -> a
extractMin t = minimum (dfsForest t)

-- Задача 6-----------------------------------------
mergeHeaps :: Ord a => Forest a -> Forest a -> Forest a
mergeHeaps hone [] = hone
mergeHeaps [] htwo = htwo
mergeHeaps (Node x xs:ts) (Node y ys:rs) | (rank (Node x xs)) < (rank (Node y ys)) = (Node x xs) : (mergeHeaps ts ((Node y ys) : rs))
                                         | (rank (Node x xs)) > (rank (Node y ys)) = (Node y ys) : (mergeHeaps ((Node x xs) : ts) rs)
                                         -- | otherwise = mergeHeaps (ts)  (combineTrees (Node x xs) (Node y ys) : rs)
                                         | otherwise = if (null ts) then mergeHeaps (combineTrees (Node x xs) (Node y ys) : ts)  (rs)
--                                                     else if (null rs) then mergeHeaps (ts)  (combineTrees (Node x xs) (Node y ys) : rs)
                                                       else mergeHeaps (ts)  (combineTrees (Node x xs) (Node y ys) : rs)
--                                         | otherwise = mergeHeaps (ts ++ [combineTrees (Node x xs) (Node y ys)])  rs
--                                         | otherwise = (combineTrees (Node x xs) (Node y ys) : ts) `mergeHeaps` rs
-- (Node x xs:ts) (Node y ys:rs)
--dfsForest (Node x []:ts) = x : dfsForest ts

--connectToEmptyHeap :: Ord a => Forest a -> Forest a -> Forest a
--connectToEmptyHeap [] htwo = undefined

-- Задача 7-----------------------------------------
insert :: Ord a => a -> Forest a -> Forest a
insert v h = mergeHeaps [Node v []] h

-- Задача 8-----------------------------------------
deleteMin :: Ord a => Forest a -> Forest a
deleteMin [] = []
deleteMin h = mergeHeaps (delMinAux h (extractMin h)) (reverse (getLeftoverHeap h (extractMin h)))
--deleteMin h = mergeHeaps (createNodesFromLeftovers (getLeftoverHeap h (extractMin h))) (delMinAux h (extractMin h))
--smallest is guaranteed to be one of the top-level elements in the heap
-- reverse (getSons foundNodeByValue)
-- mergeHeaps h (reverse (getSons foundNodeByValue))

-- aux func for getting all sons of a minimal element in a heap
getLeftoverHeap :: Ord a => Forest a -> a -> Forest a
getLeftoverHeap [] _ = []
getLeftoverHeap (Node x xs:ts) v | (x == v) = xs --(getSons (Node x xs)) -- reverse
                                 | otherwise = getLeftoverHeap ts v

createNodesFromLeftovers :: Ord a => Forest a -> Forest a
createNodesFromLeftovers h = [(Node i []) | i <- dfsForest h] --reverse ?

-- return heap without minimal element and its sons
delMinAux :: Ord a => Forest a -> a -> Forest a
delMinAux [] _ = []
delMinAux (Node x xs:ts) v | (x == v) = connectRest ts
                           | otherwise = (Node x xs) : (delMinAux ts v)
-- (x == v) && flagDelAlready == False ... where flagDelAlready = False

-- create list of independant nodes from the sons of deleted node; then add 1 by 1 (in reversed order?)

-- aux func for checking the rest of the heap and returning it in case it exists
connectRest :: Ord a => Forest a -> Forest a
connectRest ts | not (null (ts)) = ts -- && not (null (tail ts)) = (tail ts)
               | otherwise = []

-- Задача 9-----------------------------------------
binomSort :: Ord a => [a] -> [a]
binomSort [] = [] -- extractMin : deleteMin xs
binomSort (x:xs) = binomSortAux xs [(Node x [])] --(Node x []) : (binomSort xs)

binomSortAux :: Ord a => [a] -> Forest a -> [a]
binomSortAux [] [] = []
binomSortAux [] ts = extractMin ts : binomSortAux [] (deleteMin ts)
binomSortAux (x:xs) ts = binomSortAux xs (insert x ts)

-- Задача 10 -----------------------------------------
toBinary :: Forest a -> [Int]
toBinary ts = removeLeadingZeros [zeroOrOne i (getAllRanks ts) | i <- [length(dfsForest ts), length(dfsForest ts)-1..0]]
-- [rank i | i <- getSons ts]

getAllRanks :: Forest a -> [Int]
getAllRanks [] = []
getAllRanks (t:ts) = rank t : getAllRanks ts

-- get all existing sons' ranks; check presence of elements in list 'allPossibleNumOfNodes' in the res (available ranks); 0 no 1 yes.
--(lengths of all sons); find the ones existing in allPossibleNumOfNodes(compare iteratively with the allPossibleNumOfNodes); if == then 1, else - 0; remove leading 0's.
-- allPossibleNumOfNodes = [1..length(dfsForest ts)]
-- maxNodes: length(dfsForest h2)

-- aux func for removing leading zeros from list
removeLeadingZeros :: [Int] -> [Int]
removeLeadingZeros [] = []
removeLeadingZeros (x:xs) | (x == 0) = removeLeadingZeros xs
                          | otherwise = x:xs

--createResList :: [Int] -> [Int] -> [Int]
--createResList = 

zeroOrOne :: Int -> [Int] -> Int
zeroOrOne _ [] = 0
zeroOrOne x xs | (checkPresence x xs) = 1
               | otherwise = 0

-- auxiliary function to check whether an element is already present in the final list
checkPresence :: Int -> [Int] -> Bool
checkPresence n xs = if null xs then False
                     else if (n-(head xs)) == 0 then
                     True
                     else checkPresence n (tail xs)
-----------------------------------------------------  
-- Приклади деяких дерев...

t1, t2, t3, t4, t5, t6, t7, t8 :: Tree Int
--  Зауваження: t7 - результат злиття t5 і t6

-- t1 .. t4 з'являються на Мал. 1...
t1 = Node 4  []
t2 = Node 1 [Node 5 []]
t3 = Node 2 [Node 8 [Node 9 []], 
             Node 7 []]
t4 = Node 2 [Node 3 [Node 6 [Node 8 []], 
                     Node 10 []],
             Node 8 [Node 9 []],
             Node 7 []]

-- t5 і t6 зліва на Мал.2; t7 - справа на Мал.2
t5 = Node 4 [Node 6 [Node 8 []], 
                     Node 10 []]
t6 = Node 2 [Node 8 [Node 9 []], Node 7 []]
t7 = Node 2 [Node 4 [Node 6 [Node 8 []], Node 10 []],
             Node 8 [Node 9 []], 
             Node 7 []]

-- Додаткове дерево...
t8 = Node 12 [Node 16 []]

------------------------------------------------------
-- Приклади деяких куп...

h1, h2, h3, h4, h5, h6, h7 :: Forest Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 [Node 12 [Node 16 []],
              Node 5 []],
      Node 2 [Node 4 [Node 6 [Node 8 []],
                      Node 10 []],
              Node 8 [Node 9 []],
              Node 7 []]]

-- h3 показана на Мал.3...
h3 = [t1, t2, t4]

-- Дві додаткові купи використовуються далі. Вони зліва на Мал.4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 - результат злиття h4 і h5, справа на Мал.4(b)...
h6 = [Node 4 [],
      Node 1 [Node 4 [Node 6  [Node 8 []],
                      Node 10 []],
              Node 12 [Node 16 []],
              Node 5 []]]

-- h7 показана на Мал.5...
h7 = [Node 4 [Node 4 [Node 12 [Node 16 []],
                      Node 5 []],
              Node 6 [Node 8 []],
              Node 10 []]]  