module Parser where

import Text.ParserCombinators.Parsec (  Parser
                                      , parse
                                      , ParseError )
import Text.Parsec (  letter
                    , noneOf
                    , many
                    , manyTill
                    , digit
                    , char
                    , anyChar
                    , string
                    , oneOf
                    , space
                    , spaces
                    , sepBy
                    , many1
                    , skipMany1
                    , try
                   )
import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Error
-- import Control.Monad.Error.Class
-- import Control.Monad.Trans.Except (Error)
import Control.Monad.Except
import Data.List (  intersperse
                  , intercalate)
import Data.IORef


data Expr = List [Expr]
          | Number Integer
          | String String
          | Symbol String
          | Keyword String
          | Nil
          | Bool Bool
          deriving (Eq)


instance Show Expr where
    show (Bool True)     = "#t"
    show (Bool False)    = "#f"
    show (Number x)      = show x
    show Nil             = "nil"
    show (String s)      = "\"" ++ s ++ "\""
    show (Symbol s)      = s
    show (Keyword kw)    = ":" ++ kw
    show (List expr)    = "(" ++ (intercalate " " (map show expr)) ++ ")"

type Env = IORef [(String, IORef Expr)]

nullEnv :: IO Env
nullEnv = newIORef []

data LispError = NumArgs Integer [Expr]
               | TypeMismatch String Expr
               | Parser ParseError
               | BadSpecialForm String Expr
               | UnboundVar String String
               | NotFunction String String
               | Default String
               deriving (Eq)


instance Show LispError where
    show (UnboundVar msg varname) = msg ++ ": " ++ varname
    show (NumArgs n exprs) = "Expected " ++ show n
                          ++ " args; found values "
                          ++ (unwords $ map show exprs)
    show (TypeMismatch expected found) = "Invalid Type: expected "
                                      ++ expected
                                      ++ ", found " ++ show found
    show (NotFunction message func) =  message ++ ": " ++ show func
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (Default msg) = msg
    show _ = "Default Error"


instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default



type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Right val) = return val
liftThrows (Left err) = throwError err


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractVal

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var
-- isBound env varname = do
--     env2 <- readIORef env
--     case lookup varname env2 of
--         Just _ -> return True
--         _      -> return False

getVar :: Env -> String -> IOThrowsError Expr
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> Expr -> IOThrowsError Expr
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> Expr -> IOThrowsError Expr
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
    then setVar envRef var value
    else liftIO $ do
        valueRef <- newIORef value
        env      <- readIORef envRef
        writeIORef envRef ((var, valueRef) : env)
        return value

bindVars :: Env -> [(String, Expr)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
-- bindVars envRef [] = return envRef
-- bindVars envRef ((s, e):bindings) = do
--     valueRef <- newIORef e
--     env      <- readIORef envRef
--     writeIORef envRef ((s, valueRef) : env)
--     bindVars envRef bindings

trapError action = catchError action (return . show)

extractVal :: ThrowsError a -> a
extractVal (Right val) = val


nil :: Parser Expr
nil = (string "nil" <|> string "'()") >> return Nil

number :: Parser Expr
number = fmap (Number . read) (many1 digit)

false :: Parser Expr
false = string "f" >> return (Bool False)

true :: Parser Expr
true = string "t" >> return (Bool True)

boolean :: Parser Expr
boolean = char '#' >> (false <|> true)

specialChar :: Parser Char
specialChar = oneOf "!#$%&|*+-/<=>?@^_~"

symbol :: Parser Expr
symbol = do
    h <- letter <|> specialChar
    t <- many (digit <|> letter <|> specialChar)
    return $ Symbol (h:t)

keyword :: Parser Expr
keyword = do
    char ':'
    (Symbol s) <- symbol
    return $ Keyword s

anyString :: Parser Expr
anyString = do
    char '"'
    s <- manyTill anyChar (char '"')
    return $ String s

list :: Parser Expr
list = do
    char '('
    spaces
    expr <- sepBy expr spaces
    spaces
    char ')'
    return $ List expr

expr :: Parser Expr
expr = list
    <|> (try quote <|> nil)
    <|> anyString
    <|> number
    <|> boolean
    <|> keyword
    <|> symbol
    <|> anyString

quote :: Parser Expr
quote = do
    char '\''
    t <- expr
    return $ List [Symbol "quote", t]

readExpr :: String -> ThrowsError Expr
readExpr input = case parse expr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

-- readExpr :: String -> Expr
-- readExpr input = case parse expr "lisp" input of
--     Left err  -> String $ "Error found: " ++ show err
--     Right val -> val
