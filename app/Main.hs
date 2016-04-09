module Main where
import System.Environment

import Lib
import Parser

main :: IO ()
-- main = someFunc
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0)
