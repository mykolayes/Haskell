{-# OPTIONS_GHC -Wall #-}
module Yeshchenko12 where

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show, Ord)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show, Ord) 

type TypeTable = [(String, Type)]
type TEnv      = TypeTable    -- тобто [(String, Type)]
type Sub       = TypeTable    -- тобто [(String, Type)]  

-- Задача 1 -----------------------------------------
-- Передумова: Елемент, що шукається, є в таблиці
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp _ [] = error "Empty list!"
lookUp a ((k,v):xs) | a == k = v
                    | otherwise = lookUp a xs

-- Задача 2 -----------------------------------------
tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp _ b [] = b
tryToLookUp a b ((k,v):xs) | a == k = v
                           | otherwise = tryToLookUp a b xs

-- Задача 3 -----------------------------------------
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp _ [] = []
reverseLookUp b ((k,v):xs) | b == v = k : reverseLookUp b xs
                           | otherwise = reverseLookUp b xs

-- Задача 4 -----------------------------------------
occurs :: String -> Type -> Bool
occurs s (TVar str) | s == str = True
                    | otherwise = False
occurs s (TFun (TVar str) _) | s == str = True
occurs s (TFun _ (TVar str)) | s == str = True
occurs s (TFun f@(TFun _ _) _) | (occurs s f) = True
occurs s (TFun _ f@(TFun _ _)) | (occurs s f) = True
occurs _ _ = False

-- Задача 5 -----------------------------------------
-- Передумова: Немає функцій визначених користувачем (конструктор Fun)
-- Передумова: Всі змінні типів (з виразів) мають зв"язування в середовищі типів 
inferType :: Expr -> TEnv -> Type
inferType (Number _) _ = TInt
inferType (Boolean _) _ = TBool
inferType (Id s) envt = lookUp s envt
inferType (Prim p) _ = lookUp p primTypes
inferType (Cond e1 e2 e3) envt | ((inferType e1 envt) == TBool) && ((inferType e2 envt) == (inferType e3 envt)) = inferType e2 envt
                               | otherwise = TErr
inferType (App e1 e2) envt | (inferType e1 envt) /= TErr && (inferType e2 envt) == t = t'
                           | otherwise = TErr
                           where (TFun t t') = inferType e1 envt
inferType (Fun _ _) _ = TErr -- no (Fun _ _) may be passed to this function(!) - see task conditions

-- Задача 6 -----------------------------------------
applySub :: Sub -> Type -> Type
-- applySub [] t = t
-- applySub s t@(TVar str) = tryToLookUp str t s
-- applySub s (TFun t@(TVar str) x) = (TFun (tryToLookUp str t s) x)
-- applySub s (TFun x t@(TVar str)) = (TFun x (tryToLookUp str t s))
-- applySub s (TFun f@(TFun _ _) x) = (TFun (applySub s f) x)
-- applySub s (TFun x f@(TFun _ _)) = (TFun x (applySub s f))
-- applySub _ t = t

applySub s t@(TVar str) = tryToLookUp str t s
applySub s (TFun t1 t2) = (TFun (applySub s t1) (applySub s t2))
applySub _ t = t

-- Задача 7 -----------------------------------------
unify :: Type -> Type -> Maybe Sub
unify t t' = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] s = Just s
unifyPairs ((TInt, TInt):xs) s = unifyPairs xs s
unifyPairs ((TBool, TBool):xs) s = unifyPairs xs s
unifyPairs ((TVar v1, TVar v2):xs) s | (v1 == v2) = unifyPairs xs s
--                                     | otherwise = unifyPairs xs ((v2,TVar v2):(v1,TVar v1):s)
unifyPairs ((TVar v, t):xs) s | ((TVar v) /= t) && not (occurs v t) = unifyPairs xs1 ((v,t):s1)
                              | otherwise = Nothing
                              where xs1 = updateTypes [(v, t)] xs --applySub [(v, t)] xs
                                    s1 = updateSub [(v, t)] s--applySub [(v, t)] s
unifyPairs ((t, TVar v):xs) s = unifyPairs ((TVar v, t):xs) s
unifyPairs ((TFun t1 t2, TFun t1' t2'):xs) s = unifyPairs ((t1, t1') : (t2, t2') : xs) s
unifyPairs _ _ = Nothing


updateTypes :: Sub -> [(Type, Type)] -> [(Type, Type)]
updateTypes _ [] = []
updateTypes s ((t1,t2):xs) = (applySub s t1, applySub s t2) : updateTypes s xs

updateSub :: Sub -> Sub -> Sub
updateSub _ [] = []
updateSub s ((k,v):xs) = (k, applySub s v) : updateSub s xs

-- Задача 8 -----------------------------------------
inferPolyType :: Expr -> Type
inferPolyType e =
    let vx   = ['a' : show n | n <- [(1::Int)..]]
        (_, t, _) = inferPolyType' e [] vx    
    in t  

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
inferPolyType' (Number _) _ nx = ([], TInt, nx)
inferPolyType' (Boolean _) _ nx = ([], TBool, nx)
inferPolyType' (Id s) envt nx = ([], (lookUp s envt), nx)
inferPolyType' (Prim p) _ nx = ([], (lookUp p primTypes), nx)
inferPolyType' (Fun _ _) _ [] = ([],TErr,[]) --error "Empty identifiers list"
inferPolyType' (Fun x e) envt (ni:nx1) | te /= TErr = (sub1x, (TFun tp te), nx2)
                                       | otherwise = ([], TErr, [])
                                   where env1 = ((x, TVar ni):envt) 
                                         (sub1x,te,nx2) = inferPolyType' e env1 nx1 
                                         tp = applySub sub1x (TVar ni)
inferPolyType' (App _ _) _ [] = ([],TErr,[]) --error "Empty identifiers list" 
inferPolyType' (App f e) envt (ny:nx) | (unifyPairs [(applySub sub2x tf, TFun te (TVar ny))] []) /= Nothing = (subr, tr, nx2)
                                 | otherwise = ([],TErr,[])
                                   where (sub1x,tf,nx1) = inferPolyType' f envt nx
                                         (sub2x,te,(nx2)) = inferPolyType' e (updateTEnv envt sub1x) nx1 --(ny:nx2)
                                         (Just sub3x) = unifyPairs [(applySub sub2x tf, TFun te (TVar ny))] [] --unify tf (TFun te (TVar ny))
                                         subr = combineSubs [sub3x, sub2x, sub1x]
                                         tr = applySub sub3x (TVar ny)
inferPolyType' (Cond e1 e2 e3) envt nx | t1x == TBool && (t2x == t3x) = ([],t2x,nx)
                                       | otherwise = ([], TErr, [])
                                       where (_,t1x,_) = inferPolyType' e1 envt nx
                                             (_,t2x,_) = inferPolyType' e2 envt nx
                                             (_,t3x,_) = inferPolyType' e3 envt nx
--------------------------------------------------------------------
showT :: Type -> String
showT TInt        = "Int"
showT TBool       = "Bool"
showT (TFun t t') = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a)    = a
showT TErr        = "Type error"

-- Типи базових операцій (примітивів)...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

---------------------------------------------------
-- Допоміжні функції 
updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub = map modify tenv
  where modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld = sNew ++ updateTEnv sOld sNew

-- В combineSubs [s1, s2,..., sn], s1 повинна бути *самою останньою* 
-- підстановкою і повинна бути застосована *заключною*
combineSubs :: [Sub] -> Sub
combineSubs = foldr1 combine

------------------------------------------------------
-- Вивод мономорфного типу - приклади тестів...
env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Test data for applySub
sTest :: Sub
sTest = [("a",TBool),("b",TFun TBool TInt)]

------------------------------------------------------
-- Приклади для тестування уніфікації (unify)...
u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Вивод поліморфного типу - приклади тестів...
ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))
