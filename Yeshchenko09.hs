{-# OPTIONS_GHC -Wall #-}
module Yeshchenko09 where

type AttName = String
type AttValue = String
type Attribute = (AttName, [AttValue])

type Header = [Attribute]
type Row = [AttValue]
type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

-- Задача 1 -----------------------------------------
allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (_:[]) = True
allSame (x:z:xs) | x == z = allSame (z:xs)
                 | otherwise = False

-- Задача 2 -----------------------------------------
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove _ [] = []
remove key ((k,v):xs) | (key == k) = xs
                      | otherwise = (k,v) : (remove key xs)

-- Задача 3 -----------------------------------------
lookUpAtt :: AttName -> Header -> Row -> AttValue
--Передумова: Імя атрибуту присутнє в заданому заголовку.
lookUpAtt _ _ [] = error "The attributes row is empty."
lookUpAtt _ [] _ = error "No such attribute is present."
lookUpAtt an ((k,v):xs) (y:ys) | elem y zs = y
                               | otherwise = lookUpAtt an ((k,v):xs) ys
                              where zs = lookUp an ((k,v):xs) -- all possible values for given attrName (key)

lookUp ::  (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp _ [] = error "Given key is not present."
lookUp key ((k,v):xs) | (key == k) = v
                      | otherwise = lookUp key xs

-- Задача 4 -----------------------------------------
removeAtt :: AttName -> Header -> Row -> Row
removeAtt _ _ [] = error "The attributes row is empty."
removeAtt _ [] _ = error "No such attribute is present."
removeAtt an ((k,v):xs) (y:ys) | elem y zs = ys
                               | otherwise = y : (removeAtt an ((k,v):xs) ys)
                              where zs = lookUp an ((k,v):xs) -- all possible values for given attrName (key)

-- Задача 5 -----------------------------------------
buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Передумова: Кожний рядок таблиці містить одне значення заданого атрибуту
--buildFrequencyTable (k,v:xs) (h:hs,r:ys) = undefined
buildFrequencyTable (_, []) _ = []
buildFrequencyTable (k,v:xs) (h,r) = (v, num) : buildFrequencyTable (k,xs) (h,r)
                                     where num = attValueCount v (h,r)

-- aux func for counting number of appearances of a given Attribute Value in a DataSet
attValueCount :: AttValue -> DataSet -> Int
attValueCount _ (_, []) = 0
attValueCount v (h,(r:ys)) | elem v r = 1 + attValueCount v (h,ys)
                           | otherwise = attValueCount v (h,ys)

-- Задача 6 -----------------------------------------
evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _  = ""
evalTree (Leaf av) _ _ = av
evalTree (Node _ []) _ _ = ""
evalTree (Node an ((x, z):xs)) h r | x == resVal = evalTree z h r
                                   | otherwise = evalTree (Node an xs) h r
                                     where resVal = lookUpAtt an h r

-- Задача 7 -----------------------------------------
partitionData :: DataSet -> Attribute -> Partition
partitionData _ (_, []) = []
partitionData (h, r) (k,v:xs) = (v, ((removeAttHeader h k), rows)) : partitionData (h, r) (k, xs)
                                where rows = createRows (h, r) k v
                                      --hNew = removeAttHeader h k
--lookUpAtt k h r

--createDs :: DataSet -> 

createRows :: DataSet -> AttName -> AttValue -> [Row]
createRows (_, []) _ _ = []
createRows (h, (r:rs)) k v | (elem v r) = (removeAtt k h r) : createRows (h, rs) k v
                           | otherwise = createRows (h, rs) k v

removeAttHeader :: Header -> AttName -> Header
removeAttHeader [] _ = []
removeAttHeader ((k,v):xs) an | an == k = xs
                              | otherwise = (k,v): removeAttHeader xs an
-- Задача 8 -----------------------------------------
--
-- Задається...
-- В цьому простому випадку: атрибут, що вибирається - це перший атрибут в заголовку. 
--   Зауважимо, що кваліфікуючий атрибут присутній в заголовку,
--   тому його необхідно вилучити з можливих кандидатів. 
--
nextAtt :: AttSelector
--Передумова: Заголовок містить по крайній мірі один вхідний атрибут
nextAtt (headerDS, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) headerDS)

buildTree :: Attribute -> AttSelector -> DataSet -> DecisionTree 
-- buildTree _ _ (_, []) = Null --partitionData ds (atSel ds qat)
-- buildTree (_, []) _ (_, (_:_)) = Null
-- buildTree qat@(an, (v:vs)) atSel ds@(h, (x:xs)) | allSame xs = (Leaf (head x))
                                                -- | otherwise = (Node an [(av, buildTree qat atSel dsNew)])
                                                -- where px@[(av, dsNew)] = partitionData ds (atSel ds qat)
-- buildTree _ _ (_, []) = Null
-- buildTree (_, []) _ (_, (_:_)) = Null
-- buildTree qat@(an, (v:vs)) atSel ds@(h, (x:xs)) | not (null newP) = (Node anNew ([(vP, buildTree qat atSel dsP)]))
                                                -- | otherwise = Null
                                                  -- where newA@(anNew, (av:avNew)) = atSel ds qat
                                                        -- newH = removeAttHeader h an
                                                        -- newP1@[] = partitionData ds newA
                                                        -- newP@(vP, dsP):dsx = partitionData ds newA
buildTree _ _ (_,[]) = Null
buildTree qat@(an, _) atSel ds@(h, (x:xs)) | checkClAtt qat h (x:xs) = Leaf (lookUpAtt an h x)
                                              | otherwise = (Node n (createAllNodes qat atSel sons))
                                              --(Node n ([(sv, buildTree qat atSel sds)] ++ [(sv, Null)]))
-- ++ (Node n ([(sv, buildTree qat atSel ds)]))
                                              where sons@((_, _):_) = partitionData ds currAttForPart
                                                    currAttForPart@(n, _) = atSel ds qat



--aux func for checking whether the given rows all have the same value for classification attribute
checkClAtt :: Attribute -> Header -> [Row] -> Bool
checkClAtt (_, _) _ [] = True -- ?
checkClAtt (_, _) _ (_:[]) = True
checkClAtt (n, vs) h (x:xs) | (lookUpAtt n h x) == (lookUpAtt n h (head xs)) = (checkClAtt (n, vs) h xs)
                            | otherwise = False

createAllNodes :: Attribute -> AttSelector -> Partition -> [(AttValue, DecisionTree)] --[DecisionTree]
createAllNodes _ _ ([]) = []
createAllNodes qat atSel ((sv, sds):[]) = [(sv, buildTree qat atSel sds)]
createAllNodes qat atSel ((sv, sds):sp) = [(sv, buildTree qat atSel sds)] ++ createAllNodes qat atSel sp
--test func for getting the next attribute; returns the same Attribute as the usual call of the given AttSelector.
test :: Attribute -> AttSelector -> DataSet -> Attribute
test qat atSel ds = atSel ds qat

--addTupleToList ::
--createDTList ::  

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- Це таж сама таблиця, але результат у другій колонці
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]

