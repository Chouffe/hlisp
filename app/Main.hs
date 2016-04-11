module Main where
import System.Environment

import Lib
import Parser
import Evaluator
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractVal $ trapError evaled
