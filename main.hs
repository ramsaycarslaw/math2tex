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
                deriving (Show)

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
symbol x = 
  if elem x ['0'..'9'] then
    (Constant (char2float x))
  else 
    (Variable x)

-- parse the input string
parse :: String -> [Equation]
parse xs = [symbol x | x <- xs, x /= ' ']


showOp :: [Equation] -> String
showOp [] = [] 
showOp (o:os) = showo ++ ('\n' : showos)
   where showo = (show o) 
         showos = showOp os 

printEq :: [Equation] -> IO ()
printEq xs = putStr $ showOp xs

-- Add the latex enivornment around a string
dollars :: String -> String
dollars xs = "$" ++ xs ++ "$"

-- Main function
main :: IO ()
main = do 
  putStr "> "
  query <- getLine

  if query /= "q" then
    printEq ( parse query )
  else
    return ()
