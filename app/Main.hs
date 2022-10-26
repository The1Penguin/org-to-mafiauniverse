module Main where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Functor
import Data.Void
import System.Environment
import System.Exit
import Text.Megaparsec hiding (satisfy)
import Text.Megaparsec.Char
import Text.ParserCombinators.ReadP hiding (string)

type Parser = Parsec Void String

parseFile :: String -> String
parseFile =
  unlines
    . map
      ( \x -> case parse fileParser "" x of
          Left bundle -> error ("Error parsing text" ++ errorBundlePretty bundle)
          Right text -> text
      )
    . lines

fileParser :: Parser String
fileParser = undefined

main :: IO ()
main = putStr =<< parseCli =<< getArgs

parseCli :: [String] -> IO String
parseCli [] = return usage
parseCli [x] =
  if head x /= '-'
    then parseFile <$> readFile x
    else case parse argParse "" x of
      Left bundle -> fail $ "Failed parsing args" ++ errorBundlePretty bundle
      Right text -> return $ useArgs text
parseCli [x, _] = case parse argParse "" x of
  Left bundle -> fail $ "Failed parsing args" ++ errorBundlePretty bundle
  Right text -> return $ useArgs text
parseCli _ = fail "Too many arugemnts"

argParse :: Parser String
argParse =
  string "-" >> some alphaNumChar

useArgs :: String -> String
useArgs [] = ""
useArgs ('h' : _) = usage
useArgs ('v' : _) = version
useArgs (_ : xs) = useArgs xs

usage :: String
usage = "Usage: otm [-hv] [file]"

version :: String
version = "otm 0.1"
