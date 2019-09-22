{-# OPTIONS_GHC -Wall #-}
module Yeshchenko07 where
-- треба було прирівнювати поліноми [1,1] and [1,1,0,0] etc.
newtype Poly a = P [a]
-- newtype State s a = State { runState :: s -> (s, a) }
-- Задача 1 -----------------------------------------
x :: Num a => Poly a
x = P [0, 1]

-- Задача 2 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    P [] == P [] = True
    P [] == _ = False
    _ == P [] = False
    P (z:zs) == P (y:ys) = z == y && (zs) == (ys)
--    P (zs) == P (ys) = sum zs == sum ys
 
-- Задача 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where -- check if max == 0 then just 0
    show (P []) = ""
    show (P (zs)) | (all ( 0 == ) zs) = zeroIntToStr 0
                  | otherwise = if (last (take (length(drop 1 (show (showCondChk zs))) - 1) (drop 1 (show (showCondChk zs))))) == ' ' then removePlusAtEnd (take (length(drop 1 (show (showCondChk zs))) - 1) (drop 1 (show (showCondChk zs))))
                                else (take (length(drop 1 (show (showCondChk zs))) - 1) (drop 1 (show (showCondChk zs))))     

showCondChk :: (Num a, Eq a, Show a) => [a] -> String
showCondChk [] = ""
showCondChk zs | not (null (init zs)) && ((last zs) /= 1) && ((last zs) /= 0) && ((length zs)-1 /= 1) = show (last zs) ++ "x^" ++ show ((length zs)-1) ++ " + " ++ showCondChk (init zs) --usual elems (full)
               | not (null (init zs)) && ((last zs) == 1) && ((length zs)-1 /= 1) = "x^" ++ show ((length zs)-1) ++ " + " ++ showCondChk (init zs) --usual w/ coeff == 1
               | not (null (init zs)) && ((last zs) == 0) && ((length zs)-1 /= 1) = showCondChk (init zs) --usual w/ coeff == 0
               | not (null (init zs)) && ((last zs) /= 1) && ((last zs) /= 0) && ((head zs) /= 0) = show (last zs) ++ "x" ++ " + " ++ showCondChk (init zs) --pre-last elem usual, w last elem /= 0
               | not (null (init zs)) && ((last zs) /= 1) && ((last zs) /= 0) = show (last zs) ++ "x" ++ showCondChk (init zs) --pre-last elem usual, w last elem == 0
               | not (null (init zs)) && ((last zs) == 1) && ((head zs) /= 0) = "x" ++ " + " ++ showCondChk (init zs) --pre-last elem, w/ coeff == 1, w last elem /= 0
               | not (null (init zs)) && ((last zs) == 1) = "x" ++ showCondChk (init zs) --pre-last elem, w/ coeff == 1, w last elem == 0
               | not (null (init zs)) && ((last zs) == 0) = showCondChk (init zs) --pre-last elem usual, w/ coeff == 0
               | (last zs) == 0 = "" --last elem, == 0
               | otherwise = show (last zs) --last elem, > 0

-- aux func for warning-free work with (P [0]) in func 'show'
zeroIntToStr :: Int -> String
zeroIntToStr a = show a

removePlusAtEnd :: String -> String
removePlusAtEnd s = take ((length s) - 3) s

-- Задача 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) (P ys) = (P ys)
plus (P zs) (P []) = (P zs)
plus (P (zs)) (P (ys)) = P (addTwoLists zs ys)--(P ((z+y):(plus (P zs) (P ys)))) -- : plus (P zs) (P ys)

addTwoLists :: Num a => [a] -> [a] -> [a]
addTwoLists [] ys = ys
addTwoLists zs [] = zs
addTwoLists (z:zs) (y:ys) = (z+y) : (addTwoLists zs ys)

-- Задача 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P (zs)) (P (ys)) = P (mulTwoLists zs ys)
-- times (P []) _  = P [0]
-- times (P (z : zs)) (P ys) = plus (P (map (*z) ys)) (P ( (P [0]) : (times (P (zs)) (P (ys)))))

mulTwoLists :: Num a => [a] -> [a] -> [a]
--mulTwoLists _ [] = [0]
mulTwoLists [] _ = [0]
--mulTwoLists (z : zs) ys = addTwoLists (map (*z) ys) (mulTwoLists (0 : zs) ys)
mulTwoLists (z : zs) ys = addTwoLists (map (*z) ys) (0 : (mulTwoLists (zs) ys))
-- Задача 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P []) = (P [])
    negate (P (zs)) = (P (negateAux zs))
--    negate (P (z:zs)) = (P ((-1)*z)) ++ (negate (P zs))
--    negate (P (z:zs)) = (P [(-1)*z]++(negate (P zs)))
    fromInteger z = (P [(fromInteger z)])
    -- Розумних означень не існує
    abs    = undefined
    signum = undefined

-- aux func for negating array of Nums
negateAux :: Num a => [a] -> [a]
negateAux [] = []
negateAux (z:zs) = (-1)*z : negateAux zs

-- Задача 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P []) _ = 0
applyP (P (zs)) y = ((last zs) * (y^(length (init zs)))) + applyP (P (init zs)) y
--applyP (P (z:zx)) y = (z * (y^(length zx))) + applyP (P zx) y

-- Задача 8 -----------------------------------------
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
--	nderiv z y = y 
    nderiv z y | z > 0 = nderiv (z - 1) (deriv y)
               | otherwise = y

-- Задача 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    deriv (P []) = (P [])
    deriv (P (_:[])) = (P [0])
    deriv (P (zs)) = (P (reverse (derivAux zs)))

derivAux :: (Num a) => [a] -> [a]
-- derivAux :: (Num a, Differentiable a) => [a] -> [a]
derivAux [] = []
derivAux (_:[]) = []
derivAux (zs) = ((last zs)*(fromIntegral (length (init zs)))) : derivAux (init zs)
--derivAux (zs) = derivAux (init zs) ++ ((last zs)*(fromIntegral (length (init zs))))

