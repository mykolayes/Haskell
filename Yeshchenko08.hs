{-# OPTIONS_GHC -Wall #-}
module Yeshchenko08 where

data Expression = Var String                  -- Змінна
               | Val Int                      -- Ціла константа
               | Op Expression Bop Expression -- Операція
                 deriving (Show, Eq)

-- Бінарні (2-аргумента) оператори
data Bop =  Plus | Minus | Times | Divide   
          | Gt | Ge | Lt | Le| Eql
            deriving (Show, Eq)

data Statement = Assign String Expression
               | Incr String
               | If Expression Statement Statement
               | While Expression Statement       
               | For Statement Expression Statement Statement
               | Sequence Statement Statement        
               | Skip
                 deriving (Show, Eq)

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

type State = [(String, Int)]

-- Задача 1 -----------------------------------------
get ::  State -> String -> Int
get [] _ = 0
get ((str, num):xs) idx | str == idx = num
                        | otherwise = get xs idx
  
-- Задача 2 ----------------------------------------- 
extend :: State -> String -> Int -> State
extend [] idx v = [(idx, v)]
extend ((str, num):xs) idx v | str == idx = (str, v) : xs
                             | otherwise = (str, num) : (extend xs idx v)

-- Задача 3 -----------------------------------------
evalE :: State -> Expression -> Int
evalE st (Var e) = get st e
evalE _ (Val e) = e
evalE st (Op e1 bop e2) = doBop (evalE st e1) bop (evalE st e2)

-- aux func for applying bop (binary operation) to 2 ints, and returns resulting int
doBop :: Int -> Bop -> Int -> Int
doBop x bop y | bop == Plus = x + y
              | bop == Minus = x - y
              | bop == Times = x*y
              | bop == Divide = x `div` y
              | bop == Gt = fromEnum(x > y)
              | bop == Ge = fromEnum(x >= y)
              | bop == Lt = fromEnum(x < y)
              | bop == Le = fromEnum(x <= y)
              | otherwise = fromEnum(x == y) -- for the case of (bop == Eql)

-- Задача 4 -----------------------------------------
desugar :: Statement -> DietStatement
desugar (Assign str e) = DAssign str e
desugar (Incr str) = DAssign str (Op (Var str) Plus (Val 1))
desugar (If e st1 st2) = DIf e (desugar st1) (desugar st2)
desugar (While e st) = DWhile e (desugar st)
desugar (For st1 e st2 st3) = DSequence (desugar st1) (DWhile e (DSequence (desugar st3) (desugar st2)))
desugar (Sequence st1 st2) = DSequence (desugar st1) (desugar st2)
desugar (Skip) = DSkip

-- Задача 5 -----------------------------------------
evalSimple :: State -> DietStatement -> State  
evalSimple st (DAssign str e) = extend st str (evalE st e)
evalSimple st (DIf e ds1 ds2) | (evalE st e) == 0 = (evalSimple st ds2) --False
                              | otherwise = (evalSimple st ds1) --True
evalSimple st (DWhile e ds) | (evalE st e) /= 0 = evalSimple (evalSimple st ds) (DWhile e ds) --(evalSimple st ds)
                            | otherwise = st
evalSimple st (DSequence ds1 ds2) = evalSimple (evalSimple st ds1) ds2
evalSimple st (DSkip) = st

-- Задача 6 -----------------------------------------
run :: State -> Statement -> State 
run st stmnt = evalSimple st (desugar stmnt)

-- Програми -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Обчислення значення b(6) в степені e(5) в змінній out 

   { b := 6; e := 5; out:= 1;
     for (i:=0; i<e; i++) out := out*b   
   }
-}
power :: Statement
power = slist [ Assign "b" (Val 6)
              , Assign "e" (Val 5) 
              , Assign "out" (Val 1)
              , For (Assign "i" (Val 0))
                    (Op (Var "i") Lt (Var "e"))
                    (Incr "i")
                    (Assign "out" (Op (Var "out") Times (Var "b")))
              ] 

{- Обчислення цілого значення корня квадратного 
   зі значення змінної a (317) в змінній b 

   {a := 317; b := 0;
    while (a >= b*b) b++;
    b := b-1
   } 	
-}
squareRoot :: Statement
squareRoot = slist [ Assign "a" (Val 317)
                   , Assign "b" (Val 0)
                   , While (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                       (Incr "b")
                   , Assign "b" (Op (Var "b") Minus (Val 1))
                   ]

{- Обчислює значення 12-го числа Фібонначі в змінній out

  {in := 12; f0 := 1; f1 := 1;
   if (in == 0) then out := f0 else 
     if (in == 1) then out := f1 else 
       for (c := 2; c < in; c++) {
         out := f0 + f1; f0 := f1; f1 := out
       }
  }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "in" (Val 12)
                  , Assign "f0" (Val 1)
                  , Assign "f1" (Val 1)
                  , If (Op (Var "in") Eql (Val 0))
                       (Assign "out" (Var "f0"))
                       (If (Op (Var "in") Eql (Val 1))
                           (Assign "out" (Var "f1"))
                           (For (Assign "c" (Val 2))
                                (Op (Var "c") Lt (Var "in"))
                                (Incr "c")
                                (slist
                                 [ Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                 , Assign "f0" (Var "f1")
                                 , Assign "f1" (Var "out")
                                 ])
                           )
                       )
                  ]

