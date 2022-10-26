module Main where

import Control.Monad
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP

parse :: ReadP String -> String -> String
parse rules = fst . last .readP_to_S rules

main :: IO ()
main = useArgs =<< ( parse argParse . unlines) <$> getArgs

argParse :: ReadP String
argParse =
  string "-" >> (many1 (satisfy (/= ' ')))

useArgs :: String -> IO a
useArgs []       = exit
useArgs ('h':_) = usage   >> exit
useArgs ('v':_) = version >> exit
useArgs (_:xs)   = useArgs xs

usage :: IO ()
usage   = putStrLn "Usage: \"PENDINGNAME\" [-hv] [file]"
version :: IO ()
version = putStrLn "\"PENDINGNAME\" 0.1"
exit :: IO a
exit    = exitWith ExitSuccess
