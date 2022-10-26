module Main where

import System.Environment

main :: IO ()
main = putStrLn =<< readFile =<< head <$> getArgs
