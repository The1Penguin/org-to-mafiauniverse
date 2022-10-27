module Main where

import           Control.Applicative  hiding (some)
import           Control.Monad
import           Data.Functor
import           Data.Void
import           System.Environment
import           System.Exit
import           Text.Megaparsec      hiding (satisfy)
import           Text.Megaparsec.Char

type Parser = Parsec Void String

parseHelper :: Parser String -> String -> String
parseHelper parser x = case parse parser "" x of
  Left bundle -> error $ errorBundlePretty bundle
  Right text  -> text

parseFile :: String -> String
parseFile = unlines . map (parseHelper fileParser) . lines

fileParser :: Parser String
fileParser = try $
    string "* " <|>
    string "** "

parseCli :: [String] -> IO String
parseCli []     = return usage
parseCli [x]    =
  if head x /= '-'
    then parseFile <$> readFile x
    else return $ useArgs $ parseHelper argParse x
parseCli [x, _] = return $ useArgs $ parseHelper argParse x
parseCli _      = fail "Too many arugemnts"

argParse :: Parser String
argParse = string "-" >> some alphaNumChar

useArgs :: String -> String
useArgs []        = ""
useArgs ('h' : _) = usage
useArgs ('v' : _) = version
useArgs (_ : xs)  = useArgs xs

usage :: String
usage = "Usage: otm [-hv] [file]"

version :: String
version = "otm 0.1"

main :: IO ()
main = putStr =<< parseCli =<< getArgs
