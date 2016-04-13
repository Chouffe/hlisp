module Parser where

import Text.ParserCombinators.Parsec (  Parser
                                      , parse
                                      , ParseError )
import Text.Parsec (  letter
                    , notFollowedBy
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
                    , sepEndBy
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
                  , intercalate
                  , nub
                  )
import Data.IORef
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.HashMap as H

data Expr = List [Expr]
          | Vector (V.Vector Expr)
          | Set (S.Set Expr)
          | Hashmap (H.Map Expr Expr)
          | Number Integer
          | String String
          | Symbol String
          | Keyword String
          | Nil
          | Bool Bool
          | PrimitiveFunc { name :: String
                          , fn   :: [Expr] -> ThrowsError Expr
          }
          | Closure { params :: [String]
                    , vararg :: Maybe String
                    , body   :: [Expr]
                    , env    :: Env
          }

instance Eq Expr where
    (==) (Number x) (Number y) = x == y
    (==) (Bool x) (Bool y) = x == y
    (==) Nil Nil = True
    (==) (String x) (String y) = x == y
    (==) (Symbol x) (Symbol y) = x == y
    (==) (Keyword x) (Keyword y) = x == y
    -- TODO: fixme -> this is wrong
    (==) (List xs) (List ys) = and $ zipWith (==) xs ys
    (==) (Vector xs) (Vector ys) = xs == ys
    (==) (Hashmap h) (Hashmap t) = h == t
    (==) (Set xs) (Set ys) = xs == ys
    (==) PrimitiveFunc { name = name1 }
         PrimitiveFunc { name = name2 } = name1 == name2
    (==) Closure { params = ps1 , vararg = v1 , body = b1 , env = env1 }
         Closure { params = ps2 , vararg = v2 , body = b2 , env = env2 } = and [ ps1 == ps2
                                                                               , v1 == v2
                                                                               , b1 == b2
                                                                               , env1 == env2
                                                                               ]
    (==) _ _ = False

instance Show Expr where
    show (Bool True)     = "#t"
    show (Bool False)    = "#f"
    show (Number x)      = show x
    show Nil             = "nil"
    show (String s)      = "\"" ++ s ++ "\""
    show (Symbol s)      = s
    show (Keyword kw)    = ":" ++ kw
    show (List expr)     = "(" ++ (intercalate " " (map show expr)) ++ ")"
    show (Vector expr)    = "[" ++ (intercalate " " (map show (V.toList expr))) ++ "]"
    show (Hashmap h)    = "{" ++ (intercalate " " (map (\ (k, v) -> show k ++ " " ++ show v) (H.assocs h))) ++ "}"
    show (Set expr)    = "#{" ++ (intercalate " " (map show (S.elems expr))) ++ "}"
    show PrimitiveFunc {name = functionName} = "<built-in-lambda: " ++ functionName ++ ">"
    show Closure {params = args, vararg = varargs, body = body, env = env} = "(lambda (" ++ unwords (map show args)
         ++ (case varargs of
                 Nothing  -> ""
                 Just arg -> " . " ++ arg) ++ ") ...)" -- TODO: Add the body here

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
    show (Parser pe) = show pe
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

specialChar :: Parser Char
specialChar = oneOf "!#$%&|*+-/<=>?@^_~"

nilSymbol :: Parser Expr
nilSymbol = string "nil" >> notFollowedBy specialChar >> return Nil

nilEmptyList :: Parser Expr
nilEmptyList = string "'()" >> return Nil

nil :: Parser Expr
nil = nilSymbol <|> nilEmptyList

number :: Parser Expr
number = fmap (Number . read) (many1 digit)

false :: Parser Expr
false = string "f" >> return (Bool False)

true :: Parser Expr
true = string "t" >> return (Bool True)

boolean :: Parser Expr
boolean = char '#' >> (false <|> true)

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

parens :: String -> String -> Parser Expr -> ([Expr] -> Parser Expr) -> Parser Expr
parens opening closing separator constructor = do
    string opening
    spaces
    expr <- sepEndBy separator spaces
    spaces
    string closing
    constructor expr

list :: Parser Expr
list = parens "(" ")" expr (return . List)

vector :: Parser Expr
vector = parens "[" "]" expr (return . Vector . V.fromList)

set :: Parser Expr
set = parens "#{" "}" expr
            (\ expr -> if (nub expr) == expr
                       then return $ Set (S.fromDistinctAscList expr)
                       else fail ("Duplicate elements in set: #{" ++ (intercalate " " (map show expr)) ++ "}"))

hashmapEntry :: Parser Expr
hashmapEntry = do
    key <- expr
    spaces
    val <- expr
    return $ List [key, val]

hashmap :: Parser Expr
hashmap = parens "{" "}" hashmapEntry (\ keyvals -> let keys = (map (\ (List [k, _] ) -> k) keyvals)
                                                    in
                                                        if (nub keys) == keys
                                                        then return $ List [Symbol "quote", List keyvals]
                                                        else fail ("Duplicate entry in hashmap"))

expr :: Parser Expr
expr = list
        <|> vector
        <|> set
        <|> hashmap
        <|> (try nil <|> quote <|> symbol)
        <|> symbol
        <|> anyString
        <|> number
        <|> boolean
        <|> keyword
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
