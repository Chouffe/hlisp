module Parser where

import Text.ParserCombinators.Parsec (  Parser
                                      , parse )
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
import Data.List (  intersperse
                  , intercalate)

data Term = List [Term]
          | Number Integer
          | String String
          | Symbol String
          | Keyword String
          | Quote Term
          | Nil
          | Bool Bool

instance Show Term where
    show (Bool True)     = "#t"
    show (Bool False)    = "#f"
    show (Number x)      = show x
    show Nil             = "nil"
    show (String s)      = "\"" ++ s ++ "\""
    show (Symbol s)      = s
    show (Quote e)       = "'" ++ show e
    show (Keyword kw)    = ":" ++ kw
    show (List terms)    = "(" ++ (intercalate " " (map show terms)) ++ ")"

nil :: Parser Term
nil = (string "nil" <|> string "'()") >> return Nil

number :: Parser Term
number = fmap (Number . read) (many1 digit)

false :: Parser Term
false = string "f" >> return (Bool False)

true :: Parser Term
true = string "t" >> return (Bool True)

boolean :: Parser Term
boolean = char '#' >> (false <|> true)

specialChar :: Parser Char
specialChar = oneOf "!#$%&|*+-/<=>?@^_~"

symbol :: Parser Term
symbol = do
    h <- letter <|> specialChar
    t <- many (digit <|> letter <|> specialChar)
    return $ Symbol (h:t)

keyword :: Parser Term
keyword = do
    char ':'
    (Symbol s) <- symbol
    return $ Keyword s

anyString :: Parser Term
anyString = do
    char '"'
    s <- manyTill anyChar (char '"')
    return $ String s

list :: Parser Term
list = do
    char '('
    spaces
    terms <- sepBy term spaces
    spaces
    char ')'
    return $ List terms

term :: Parser Term
term = list
    <|> (try nil <|> quote)
    <|> anyString
    <|> number
    <|> boolean
    <|> keyword
    <|> symbol
    <|> anyString

quote :: Parser Term
quote = do
    char '\''
    t <- term
    return $ Quote t

readExpr :: String -> String
readExpr input = case parse term "lisp" input of
    Left err  -> "No match:" ++ show err
    Right val -> "Found value: " ++ show val
