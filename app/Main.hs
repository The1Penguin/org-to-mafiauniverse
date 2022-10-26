module Main where

import Control.Monad
import Data.Functor
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP



parse :: ReadP String -> String -> String
parse rules = fst . last .readP_to_S rules

main :: IO ()
main = putStrLn =<< helper =<< getArgs

helper :: [String] -> IO String
helper []       = return usage
helper (x:[])   = if (head x /= '-')
                    then readFile x
                    else return $ useArgs $ parse argParse x
helper (x:_:[]) = return $ useArgs $ parse argParse x
helper _        = fail "Too many arugemnts"


argParse :: ReadP String
argParse =
  string "-" >> (many1 (satisfy (/= ' ')))

useArgs :: String -> String
useArgs []       = ""
useArgs ('h':_)  = usage
useArgs ('v':_)  = version
useArgs (_:xs)   = useArgs xs

usage :: String
usage   = "Usage: otm [-hv] [file]"
version :: String
version = "otm 0.1"
