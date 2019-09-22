{-# OPTIONS_GHC -Wall #-}
module Yeshchenko11 where

-- import Data.Stream

data Stream a = Cons a (Stream a)

-- Екземпляр Show виводить перші 20 елементів, за якими розташовані крапки продовження
instance Show a => Show (Stream a) where
    show xs =  (foldl (++) "[" 
                  $ map (\x -> (show x) ++ ", ") $ take 20 $ streamToList xs
                ) ++ "..."

--Задача 1 -----------------------------------------
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Задача 2 -----------------------------------------
instance Functor Stream where
    fmap f (Cons x xs) = (Cons (f x) (fmap f xs))

-- Задача 3 -----------------------------------------
sRepeat :: a -> Stream a
sRepeat v = (Cons v (sRepeat v))

sIterate :: (a -> a) -> a -> Stream a
sIterate f v = (Cons v (sIterate f (f v)))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) (Cons y ys) = (Cons x (Cons y (sInterleave xs ys)))

sTake :: Int -> Stream a -> [a]
sTake n (Cons x xs) | n > 0 = x : sTake (n-1) xs
                    | otherwise = []

-- Задача 4 -----------------------------------------
nats :: Stream Integer
nats = sIterate (1+) 0

-- Задача 5 -----------------------------------------
ruler :: Stream Integer
ruler = sIterateCustom findMaxPwr 1

-- aux func for the 'ruler' function, which helps get the correct value of pwrOf2 for the current n in [1,2..] (see division rules for the exercise)
sIterateCustom :: (Integer -> Integer -> Integer -> Integer) -> Integer -> Stream Integer
sIterateCustom f v = (Cons (f v 0 0) (sIterateCustom f (v+1)))

-- natsFromOne :: Stream Integer
-- natsFromOne = sIterate (1+) 1

-- aux func: the 1 param is Stream of [1, 2..] ; it returns the Stream of highest power of 2, by which the current num can be divided without the remainder (0 if such power of 2 does not exist)

-- findMaxPwr :: Stream Integer -> Stream Integer
-- findMaxPwr (Cons x xs) = undefined

-- aux func for finding the highest acceptable value of power of 2
-- the 1 param is the current num from [1,2..]; the 2 param (should always start with 0) is the current highest pwr of 2; the 3 param (should always start with 0 as 2^0 = 1) is the current value of pwr of 2, which is being evaluated (and will replace the 2 param if successful)
-- call example: findMaxPwr 6 0 0 => 1
findMaxPwr :: Integer -> Integer -> Integer -> Integer
findMaxPwr n mv cv | (2^cv <= n) && (rem n 2^cv == 0) && (checkDiv n (2^cv)) = findMaxPwr n cv (cv+1)
                   | (2^cv < n) = findMaxPwr n mv (cv+1)
                   | otherwise = mv

checkDiv :: Integer -> Integer -> Bool
checkDiv n currDiver | (n - currDiver > 0) = checkDiv (n - currDiver) currDiver
                     | (n - currDiver == 0) = True
                     | otherwise = False
-- Задача 6 -----------------------------------------
rand :: Integer -> Stream Integer
rand r = (Cons r (rand (lcg 1103515245 r 12345 2147483648)))  

--rand aux function - lcg (linear congruential generator)
lcg :: Integer -> Integer -> Integer -> Integer -> Integer
lcg r a c m = mod (a * r + c) m

-- Задача 7 -----------------------------------------
fib :: Integer -> Integer
fib n = fibAux n 1 1

fibAux :: Integer -> Integer -> Integer -> Integer
fibAux 0 1 1 = 1
fibAux 1 1 1 = 1
fibAux n numOne numTwo | n >= 2 = fibAux (n-1) numTwo (numOne+numTwo)
                       | otherwise = numTwo

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Задача 8 -----------------------------------------
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Задача 9 -----------------------------------------
data  Matrix a = M(a,a)(a,a)
         deriving (Show, Eq, Ord)
         
instance Num a => Num (Matrix a) where
    (+) (M(a,b)(c,d)) (M(e,f)(g,h)) = M(a+e,b+f)(c+g,d+h)
    (*) (M(a,b)(c,d)) (M(e,f)(g,h)) = M(a*e + b*g,a*f + b*h)(c*e + d*g,c*f + d*h)
    negate (M(a,b)(c,d)) = (M(-a,-b)(-c,-d))
    fromInteger n = M(fromInteger n,fromInteger 0)(fromInteger 0,fromInteger n)
    -- Реалізовувати не потрібно
    abs    = undefined
    signum = undefined

-- Задача 10 ----------------------------------------
fastFib :: Integer -> Integer
fastFib n = b where (M(_,b)(_,_)) = (M(1,1)(1,0))^n--fastFibAux n (M(1,1)(1,0)) (M(1,1)(1,0))

-- fastFibAux :: Integer -> Matrix Integer -> Matrix Integer -> Integer
-- fastFibAux n m1@(M(a,b)(c,d)) m2@(M(e,f)(g,h)) | n > 0 = fastFibAux (n-1) (m1*m2) m2
                                               -- | otherwise = f