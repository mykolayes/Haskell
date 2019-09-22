{-# OPTIONS_GHC -Wall #-}
module Yeshchenko10 where

import Text.ParserCombinators.Parsec
import Data.List.Split
import Text.Read

-- Задача 1 -----------------------------------------
numbers  :: String -> Maybe [Double]
-- numbers = undefined
numbers [] = Just []
numbers str | allAreDouble (strToSubstrs str) = Just (map read (strToSubstrs str))
            | otherwise = Nothing

-- aux func to break up string separated by ';' into a list of substrings, which hopefully all can be casted to Double
strToSubstrs :: String -> [String]
strToSubstrs str = splitOn ";" str

-- aux func for checking whether conversion of String to Double is possible
convertStrToDouble :: String -> Maybe Double
convertStrToDouble str = readMaybe str :: Maybe Double

-- aux func for checking whether all elements of the list can be casted to Double
allAreDouble :: [String] -> Bool
allAreDouble [] = True
allAreDouble (x:xs) = case convertStrToDouble x of
                        Just _ -> allAreDouble xs
                        Nothing -> False
-- Задача 2 ----------------------------------------- 
balance  :: String -> String
balance str | (countBraces (getBraces str) []) = "Is balanced"
            | otherwise = "Not balanced"

-- aux func for creating a String of braces (in the order they appear) from a String of different characters
getBraces :: String -> String
getBraces [] = []
getBraces (x:xs) | x == '(' || x == ')' || x == '[' || x == ']' || x == '{' || x == '}' = x : getBraces xs
                 | otherwise = getBraces xs

-- aux func for counting amount of 3 different braces types (round, square and curly) based on a String of only braces; returns Bool, which indicates, whether the braces open and close in a right order
-- param [Integer] is a stack of numbers (1/2/3, which indicate the round, square and curly brace respectively) representing the list of open brace and their type; the one on top is the last one opened
countBraces :: String -> [Integer] -> Bool
countBraces [] [] = True
countBraces [] _ = False
countBraces (x:xs) (ys) | (x == '(') = countBraces xs (1 : ys)
                        | (x == '[') = countBraces xs (2 : ys)
                        | (x == '{') = countBraces xs (3 : ys)
                        | (x == ')') && (head ys == 1) = countBraces xs (tail ys)
                        | (x == ']') && (head ys == 2) = countBraces xs (tail ys)
                        | (x == '}') && (head ys == 3) = countBraces xs (tail ys)
                        | otherwise = False
--                        | otherwise = error "The array must include only braces (please use the function getBraces on your String first!"

----------------  Мова SPL  ------------   
data Expression =
    Var String                   -- Змінна
  | Val Int                      -- Ціла константа
  | Op Expression Bop Expression -- Операція
  deriving (Show, Eq)

-- Бінарні (2-аргумента) оператори
data Bop = Plus | Minus | Times | Divide   
         | Gt | Ge | Lt | Le | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)                
-----------------
-- лексика
-----------------
-- number = digit { digit }.
-- iden   = char {digit | char}.   not "if" "while" "for" "else"
-- digit  = "0" | "1" | ... | "8" | "9".
-- addOp  = "+" | "-".
-- mulOp  = "*" | "/".
-- relOp  = "<" | "<=" | ">" | ">=" | "==" 
-- symbol = ';' | '{' | '}' | '(' | ')'
-- keyword = "if" | "while" | "for" | "else"
----------------
identifier :: Parser String
identifier = try( do {name <- iden;
                      if (any(name==) ["for","if","else","while"])
                         then unexpected ("reserved word " ++ show name)
                         else return name 
                     } )          
oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop   
mulOp = (oper "*" Times) <|> (oper "/" Divide)

-- розпізнати всі "порожні" символи в кінці		
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

--   :type Op -----> Expression -> Bop -> Expression -> Expression 
--   :type flip Op -------> Bop -> Expression -> Expression -> Expression         
exprOp :: Parser Bop -> Parser (Expression -> Expression -> Expression)
exprOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char -> Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum))  

-- Задача 3 -----------------------------------------
iden :: Parser String
iden =  do
          c <- letter
          cs <- many (letter<|>digit)
          return (c:cs) 
          <?>
        "iden"

number :: Parser Int
number =  do
            s <- string "-" <|> return []
            cs <- many1 digit
            return $ read (s ++ cs) 
            <?>
          "number"
 
addOp :: Parser Bop  
addOp = try (oper "+" Plus ) <|> try (oper "-" Minus ) <?> "addOp"

relOp :: Parser Bop  
relOp = try (oper ">=" Ge ) <|> try (oper ">"  Gt ) <|> try (oper "<=" Le ) <|> try (oper "==" Eql ) <|> try (oper "<"  Lt ) <?> "relOp"

-----------------
-- вирази
-----------------
-- expr   = simple [relOp simple]
-- simple = term { addOp term }.
-- term   = factor { mulOp factor }.
-- factor = "(" expr ")" | number | identifier .
----------------	
factor :: Parser Expression
factor = do { symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Val nm)}
     <|> do {cs <- lexem identifier; return (Var cs) } 
     <?> "factor" 

-- Задача 4 -----------------------------------------
term :: Parser Expression     
term = chainl1 factor (exprOp mulOp) <?> "term"  

simple :: Parser Expression
simple = chainl1 term (exprOp addOp) <?> "simple" 

expr :: Parser Expression
expr = chainl1 simple (exprOp relOp) <?> "expr"

-----------------
-- оператори
-----------------
-- stmt  = "for" forSt | "while" whileSt |
--         "if" ifSt | iden assSt | '{' listSt '}' 
-- forSt = '(' stmt ';' expr ';' stmt ')' stmt 
-- whileSt = '(' expr ')' stmt 
-- ifSt  = '(' expr ')' stmt "else" stmt 
-- assSt = "++" | ":=" expr 
-- listSt = [stmt] {';' [stmt]}   
-----------------
stmt :: Parser Statement 
stmt = do {keyword "for"; forSt}
       <|> do {keyword "while"; whileSt}
       <|> do {keyword "if"; ifSt}
       <|> do {var <- lexem identifier; assignSt var}
       <|> do {symbol '{'; s <- listSt; symbol '}'; return s }
       <?> "statement"

-- Задача 5 -----------------------------------------
forSt :: Parser Statement 
forSt = do 
          symbol '('
          sInit <- stmt
          symbol ';'
          con <- expr
          symbol ';'
          sIter <- stmt
          symbol ')'
          body <- stmt 
          return (For sInit con sIter body) <?> "forSt"

whileSt :: Parser Statement               
whileSt = do 
            symbol '('
            con <- expr
            symbol ')'
            s <- stmt
            return (While con s) <?> "whileSt" 
              
ifSt :: Parser Statement               
ifSt    = do 
          symbol '('
          con <- expr
          symbol ')'
          body1 <- stmt
          keyword "else"
          body2 <- stmt
          return (If con body1 body2) <?> "ifSt"  
              
listSt :: Parser Statement               
listSt = chainl1 stmt semiColon

semiColon :: Parser (Statement -> Statement -> Statement)
semiColon = try (symbol ';') >> return Sequence

assignSt :: String -> Parser Statement 
assignSt xs = do 
                  symbol ':'
                  symbol '='
                  e <- expr
                  return (Assign xs e)
            <|> do 
                  symbol '+'
                  symbol '+'
                  return (Incr xs)
               
---------------------------------------------	
-- Головні функції
---------------------------------------------				
contents :: Parser a -> Parser a
contents p = do {spaces; r<-p; eof; return r}

parseMain :: String -> Either ParseError Statement 
parseMain s = parse (contents  stmt) "" s

---------------------------------------------
--- Дані для тестування
--------------------------------------------- 
power :: String
power =
   "{ b := 6; e := 5; out:= 1;\
   \  for (i:=0; i<e; i++) out := out*b   \
   \}"

squareRoot :: String
squareRoot =
   "{a := 317; b := 0;\
   \  while (a >= b*b) b++;\
   \  b := b-1\
   \ }"