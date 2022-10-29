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
  Spoiler [Info]

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
             Spoiler inf        -> "[SPOILER]" ++ concatMap show inf ++ "[/SPOILER]"

newtype Post = Post [Info]

instance Show Post where
  show :: Post -> String
  show (Post a) = concatMap show a

reduce :: [Info] -> [Info]
reduce []                               = []
reduce ((List x) : (List y) : xs)       = reduce $ List (x++y) : xs
reduce ((Spoiler x) : (Spoiler y) : xs) = Spoiler (x++y) : reduce xs
reduce ((Spoiler x) : y : xs)           = reduce $ Spoiler (reduce $ x ++ [y]) : xs
reduce (x:xs)                           = x: reduce xs

parseHelper :: Parser a -> String -> a
parseHelper parser x = case parse parser "" x of
  Left bundle -> error $ errorBundlePretty bundle
  Right text  -> text

parseFile :: String -> Post
parseFile = Post . reduce . map (parseHelper fileParser) . dropWhile (/= "")  . lines

fileParser :: Parser Info
fileParser =
   try $
   choice [
     Header                     <$> (string' "* "   >> takeRest),
     Subheader                  <$> (string' "** "  >> takeRest),
     Subsubheader               <$> (string' "*** " >> takeRest),
     List . (: []) . Bread      <$> (string' "- "   >> takeRest),
     -- Image and video needs to know if it is a image or video
     Image . URL                <$> between (string' "[[") (string' "]]") (takeWhileP Nothing (/= ']')),
     Video . URL                <$> between (string' "[[") (string' "]]") (takeWhileP Nothing (/= ']')),
     -- parseLink,
     Spoiler . (: []) . Bread   <$> (string' "#+BEGIN" >> return ""),
     Spoiler . (: []) . Bread   <$> (string' "#+END"   >> return ""),
     Bread                      <$> takeRest
     ]

parseLink :: Parser Info
parseLink = undefined

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
