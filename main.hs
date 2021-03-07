module Main (main) where 
import Lexer ( Equation (..), tokenise, printEq, joinConst )
import Data.Char

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
    Lexer.printEq ( Lexer.joinConst (Lexer.tokenise query )) ;
    main
    }
  else
    return ()
