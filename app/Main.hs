module Main where

import           Control.Applicative  hiding (some)
import           Control.Monad
import           Data.Functor
import           Data.Void
import           GHC.Stack.CCS        (whereFrom)
import           GHC.StaticPtr        (StaticPtrInfo)
import           System.Environment
import           System.Exit
import           Text.Megaparsec      hiding (satisfy)
import           Text.Megaparsec.Char

type Parser = Parsec Void String

newtype URL = URL String

data Info =
  Bread String        |
  List [Info]         |
  Header String       |
  Subheader String    |
  Subsubheader String |
  Image URL           |
  Video URL           |
  Link URL Info       |
  Spoiler Info

instance Show Info where
  show :: Info -> String
  show info = case info of
             Bread text         -> text
             List texts         -> "[LIST]" ++ concatMap show texts ++ "[/LIST]"
             Header text        -> "[TITLE]" ++ text ++ "[/TITLE]"
             Subheader text     -> "[SIZE=4]" ++ text ++ "[/SIZE]"
             Subsubheader text  -> "[SIZE=2]" ++ text ++ "[/SIZE]"
             Image (URL url)    -> "[IMG]" ++ url ++ "[/IMG]"
             Video (URL url)    -> "[VIDEO]" ++ url ++ "[/VIDEO]"
             Link (URL url) inf -> "[URL=\"" ++ url ++ "\"]" ++ show inf  ++ "[/IMG]"
             Spoiler inf        -> "[SPOILER]" ++ show inf ++ "[/SPOILER]"

newtype Post = Post [Info]

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
