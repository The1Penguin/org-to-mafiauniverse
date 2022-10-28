module Main where

import           Control.Applicative  hiding (some)
import           Control.Monad
import           Data.Functor
import           Data.Void
import           System.Environment
import           System.Exit
import           Text.Megaparsec
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
             List texts         -> "[LIST]" ++ concatMap ((++) "[*] " . show) texts ++ "[/LIST]"
             Header text        -> "[TITLE]" ++ text ++ "[/TITLE]"
             Subheader text     -> "[SIZE=4]" ++ text ++ "[/SIZE]"
             Subsubheader text  -> "[SIZE=2]" ++ text ++ "[/SIZE]"
             Image (URL url)    -> "[IMG]" ++ url ++ "[/IMG]"
             Video (URL url)    -> "[VIDEO]" ++ url ++ "[/VIDEO]"
             Link (URL url) inf -> "[URL=\"" ++ url ++ "\"]" ++ show inf  ++ "[/IMG]"
             Spoiler inf        -> "[SPOILER]" ++ show inf ++ "[/SPOILER]"

newtype Post = Post [Info]

instance Show Post where
  show :: Post -> String
  show (Post a) = concatMap show a

parseHelper :: Parser a -> String -> a
parseHelper parser x = case parse parser "" x of
  Left bundle -> error $ errorBundlePretty bundle
  Right text  -> text

parseFile :: String -> Post
parseFile = Post . map (parseHelper fileParser) . lines

fileParser :: Parser Info
fileParser = undefined
--   try $
--     string' "* "       <|>
--     string' "** "      <|>
--     string' "*** "     <|>
--     string' "- "       <|>
--     string' "[["       <|>
--     string' "#+BEGIN"  <|>
--     string' "#+END"

parseCli :: [String] -> IO Post
parseCli []     = return $ Post [usage]
parseCli [x]    =
  if head x /= '-'
    then parseFile <$> readFile x
    else return $ useArgs $ parseHelper argParse x
parseCli [x, _] = return $ useArgs $ parseHelper argParse x
parseCli _      = fail "Too many arugemnts"

argParse :: Parser String
argParse = string "-" >> some alphaNumChar

useArgs :: String -> Post
useArgs []        = Post [Bread ""]
useArgs ('h' : _) = Post [usage]
useArgs ('v' : _) = Post [version]
useArgs (_ : xs)  = useArgs xs

usage :: Info
usage = Bread "Usage: otm [-hv] [file]"

version :: Info
version = Bread "otm 0.1"

main :: IO ()
main = putStr . show =<< parseCli =<< getArgs
