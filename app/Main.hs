module Main where

import Control.Applicative
import Control.Monad
import Data.Functor
import System.Environment
import System.Exit
import Text.Megaparsec hiding (parse, satisfy)
import Text.ParserCombinators.ReadP

parse :: ReadP String -> String -> String
parse rules = fst . last . readP_to_S rules

parseFile :: String -> String
parseFile = unlines . map (parse fileParser) . lines

fileParser :: ReadP String
fileParser = undefined

main :: IO ()
main = putStr =<< parseCli =<< getArgs

parseCli :: [String] -> IO String
parseCli [] = return usage
parseCli [x] =
  if head x /= '-'
    then parseFile <$> readFile x
    else return $ useArgs $ parse argParse x
parseCli [x, _] = return $ useArgs $ parse argParse x
parseCli _ = fail "Too many arugemnts"

argParse :: ReadP String
argParse =
  string "-" >> many1 (satisfy (/= ' '))

useArgs :: String -> String
useArgs [] = ""
useArgs ('h' : _) = usage
useArgs ('v' : _) = version
useArgs (_ : xs) = useArgs xs

usage :: String
usage = "Usage: otm [-hv] [file]"

version :: String
version = "otm 0.1"
