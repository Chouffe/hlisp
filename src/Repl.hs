module Repl where

import Control.Monad
import Evaluator
import Parser
import System.IO
import Data.IORef


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runRepl2 :: IO Env -> [(String, String)] -> IO ()
runRepl2 env history = do
    result <- (readPrompt "Lisp > ")
    case result of
        ""     -> runRepl2 env history
        "quit" -> return ()
        expr   -> do bindings <- env
                     let e = readExpr expr
                     evaled   <- evalString bindings expr
                     putStrLn evaled
                     ref      <- readIORef bindings
                     valStar1 <- newIORef (extractVal e)
                     runRepl2 (newIORef (("*1", valStar1) : ref))
                              ((expr, evaled) : history)
