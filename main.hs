import Data.Char

-- all of the types
data Equation = Constant Float 
                | Variable Char 
                | Carat 
                | Slash 
                | Star 
                | Plus 
                | Minus 
                | LParen 
                | RParen  
                | Dot
                deriving (Show, Eq)

-- util function
char2float :: Char -> Float
char2float n = fromInteger (read [n])

-- simple pattern matching
symbol :: Char -> Equation
symbol '(' = LParen
symbol ')' = RParen
symbol '+' = Plus
symbol '-' = Minus
symbol '*' = Star
symbol '/' = Slash
symbol '^' = Carat
symbol '.' = Dot
symbol x = 
  if elem x ['0'..'9'] then
    (Constant (char2float x))
  else 
    (Variable x)

-- tokenise the input string
tokenise :: String -> [Equation]
tokenise xs = [symbol x | x <- xs, x /= ' ']


-- convert a list of type equation to a string
showOp :: [Equation] -> String
showOp [] = [] 
showOp (o:os) = showo ++ ('\n' : showos)
   where showo = (show o) 
         showos = showOp os 

-- print an equation
printEq :: [Equation] -> IO ()
printEq xs = putStr $ showOp xs

-- returns true if an equation is a constant
isConst :: Equation -> Bool
isConst (Constant _) = True
isConst _ = False

getConst :: Equation -> Float
getConst (Constant x) = x

-- joins consecutive constants
joinConst :: [Equation] -> [Equation]
jointConst [] = []
joinConst (x:xs) = 
  if xs == [] then
    [x]
  else if isConst (head xs) && isConst x then
      joinConst ([(Constant ((getConst x) * 10 + (getConst (head xs))))] ++ (tail xs))
    else
      [x] ++ joinConst xs
         
-- Add the latex enivornment around a string
dollars :: String -> String
dollars xs = "$" ++ xs ++ "$"

-- Main function
main :: IO ()
main = do 
  putStr "> "
  query <- getLine

  if query /= "q" then
    do {
    printEq ( joinConst (tokenise query )) ;
    main
    }
  else
    return ()
