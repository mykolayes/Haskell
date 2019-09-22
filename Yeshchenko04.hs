{-# OPTIONS_GHC -Wall #-}
module Yeshchenko04 where

-- type Forest a = [Tree a]

import Data.Tree


data BTree a = BEmpty | BNode a (BTree a) (BTree a)
               deriving (Show, Eq)

-- Задача 1 -----------------------------------------
dfsForest ::  Forest a -> [a]
dfsForest [] = []
dfsForest (Node x []:ts) = x : dfsForest ts
dfsForest (Node x xs:ts) = x : (dfsForest xs) ++ dfsForest ts

-- Задача 2 ----------------------------------------- 
bfsForest ::  Forest a -> [a]
bfsForest [] = []
bfsForest (Node x xs:ts) = x : (bfsForest (ts ++ xs))

-- Задача 3 -----------------------------------------
isInTree :: (Eq a) => Tree a -> Tree a -> Bool
--isInTree (Node x xs) (Node y ys) = undefined
isInTree a b = equalTree a b || isAtLeastOneTrue (map (isInTree a) (getSons b)) where
    isAtLeastOneTrue [] = False
    isAtLeastOneTrue (x:xs)
        |x==False = False || isAtLeastOneTrue xs
        |otherwise = True

-- aux func for equality checking
equalTree :: (Eq a) => Tree a -> Tree a -> Bool
equalTree (Node x xs) (Node y ys) = x==y && isAllTrue (zipWith equalTree xs ys) where
    isAllTrue [] = True
    isAllTrue (z:zs)
        |z==True = True && isAllTrue zs
        |otherwise = False

-- aux func for getting list of sons
getSons :: Tree a -> Forest a
getSons (Node _ xs) = xs

-- aux func for getting value
getValue :: Tree a -> a
getValue (Node x _) = x

-- Задача 4 -----------------------------------------
toBTree :: Forest a -> BTree a
toBTree [] = BEmpty
toBTree (x:xs) = BNode (getValue x) (toBTree (getSons x)) (toBTree xs)
--toBTree (Node x xs:ts) = BNode x (toBTree (getSons xs)) (toBTree ts)

-- Задача 5 -----------------------------------------
fromBTree :: BTree a -> Forest a  
fromBTree BEmpty = []
fromBTree (BNode value lSon rSon) = Node value (fromBTree (lSon)) : (fromBTree rSon)

-- Задача 6 -----------------------------------------
isSearch :: (Ord a) => BTree a -> Bool
isSearch a
    |isEmpty a = True
    |isEmpty (getLeft a) && isEmpty (getRight a) = True
    |isEmpty (getLeft a) = (getNode a < getNode(getRight a))&&isSearch(getLeft a)&&isSearch(getRight a)
    |isEmpty (getRight a) = (getNode a > getNode(getLeft a))&&isSearch(getLeft a)&&isSearch(getRight a)
    |otherwise = (getNode a > getNode(getLeft a))&&(getNode a < getNode(getRight a))&&isSearch(getLeft a)&&isSearch(getRight a)

-- aux func for getting node + l and r sons of btree
getNode::BTree a -> a
getNode BEmpty = error "No node."
getNode (BNode a _ _) = a

getLeft::BTree a -> BTree a
getLeft BEmpty = error "No child."
getLeft (BNode _ x _) = x

getRight::BTree a -> BTree a
getRight BEmpty = error "No child."
getRight (BNode _ _ x) = x

isEmpty :: BTree a -> Bool
isEmpty BEmpty = True
isEmpty _ = False

-- Задача 7  -----------------------------------------
elemSearch ::(Ord a) => BTree a -> a -> Bool
elemSearch tr v = v `elem` (bfsForest (fromBTree tr))

-- Задача 8 ------------------------------------------
insSearch :: (Ord a) => BTree a -> a -> BTree a 
insSearch tr v 
    |isEmpty(tr) = (BNode v BEmpty BEmpty)
    |v<=getNode tr = (BNode (getNode(tr)) (insSearch(getLeft tr) v) (getRight(tr)))
    |v>getNode tr = (BNode (getNode(tr)) (getLeft(tr)) (insSearch(getRight tr) v))
    |otherwise = tr

-- Задача 9 ------------------------------------------
delSearch :: (Ord a) => BTree a -> a -> BTree a
delSearch tr v
    |isEmpty(tr) = tr
    |v<getNode(tr) = (BNode (getNode tr) (delSearch (getLeft tr) v) (getRight tr))
    |v>getNode(tr) = (BNode (getNode tr) (getLeft tr) (delSearch (getRight tr) v))
    |v==getNode(tr)&&isEmpty(getLeft tr)&&isEmpty(getRight tr) = BEmpty
    |v==getNode(tr)&&isEmpty(getLeft tr) = getRight(tr)
    |v==getNode(tr)&&isEmpty(getRight tr) = getLeft(tr)
    |otherwise = (BNode (getNode(getLeft(tr))) (delSearch (getLeft(tr)) (getNode(getLeft tr))) (getRight tr))

-- Задача 10 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList l = (trIntoList [] (foldl (insSearch) BEmpty l)) where
    trIntoList res tr
        |isEmpty(tr) = res
        |isEmpty(getLeft tr)&&isEmpty(getRight tr) = res++[(getNode tr)]
        |isEmpty(getLeft tr) = [getNode tr]++(trIntoList res (getRight tr))
        |isEmpty(getRight tr) = (trIntoList res (getLeft tr))++[getNode tr]
        |otherwise = (trIntoList res (getLeft tr))++[getNode tr]++(trIntoList res (getRight tr))

--- Впорядковане дерево
otx  :: [Tree Int]
otx =  [Node 1 [Node 2 [], 
                Node 3 [Node 10 []]] ,
        Node 4 [Node 5 [Node 8 []], 
                Node 6 [Node 9 []],
                Node 7 []] 
       ] 
---  Бінарні дерева 
bt, bt1, bt2 ::  BTree Int
bt = BNode 1 (BNode 2 BEmpty
                      (BNode 3 (BNode 10 BEmpty BEmpty)
                                BEmpty)
             ) 
             (BNode 4 (BNode 5 (BNode 8  BEmpty BEmpty)
                               (BNode 6  (BNode 9 BEmpty BEmpty)
                                         (BNode 7 BEmpty BEmpty)
                               )
                      )
              BEmpty
             )
bt1 = BNode 9 (BNode 4 BEmpty 
                       (BNode 8 BEmpty BEmpty))
              (BNode 20 (BNode 10 BEmpty BEmpty) 
                        BEmpty)
bt2 = BNode 9 (BNode 4 BEmpty 
                       (BNode 8 BEmpty 
                                (BNode 9 BEmpty BEmpty)))
              (BNode 20 (BNode 10 BEmpty BEmpty) 
                        BEmpty)




