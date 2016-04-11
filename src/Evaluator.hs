{-# LANGUAGE ExistentialQuantification #-}
module Evaluator where

import Control.Monad
import Control.Monad.Error
import Parser

unpackNum :: Expr -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: Expr -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: Expr -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

boolBinop :: (Expr -> ThrowsError a) -> (a -> a -> Bool) -> [Expr] -> ThrowsError Expr
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numericBinop :: (Integer -> Integer -> Integer) -> [Expr] -> ThrowsError Expr
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op exprs = mapM unpackNum exprs >>= return . Number . foldl1 op

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

evalExpr :: [Expr] -> ThrowsError Expr
evalExpr (expr:_) = eval expr

isString :: [Expr] -> ThrowsError Expr
isString ((String _ ) : []) = return $ Bool True
isString _ = return $ Bool False

isSymbol :: [Expr] -> ThrowsError Expr
isSymbol ((Symbol _ ) : []) = return $ Bool True
isSymbol _ = return $ Bool False

isNumber :: [Expr] -> ThrowsError Expr
isNumber ((Number _) : []) = return $ Bool True
isNumber _ = return $ Bool False

cdr :: [Expr] -> ThrowsError Expr
cdr [List (x : xs )] = return $ List xs
cdr badArg = throwError $ NumArgs 1 badArg

car :: [Expr] -> ThrowsError Expr
car [List (x : xs)] = return x
car badArg = throwError $ NumArgs 1 badArg

cons :: [Expr] -> ThrowsError Expr
cons [expr, (List exprs)] = return $ List (expr : exprs)
cons badArg = throwError $ NumArgs 2 badArg

eqv :: [Expr] -> ThrowsError Expr
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

primitives :: [(String, [Expr] -> ThrowsError Expr)]
primitives = [ ("eval", evalExpr)

             -- List Primitives
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)

             -- Equality
             , ("eq?", eqv)
             , ("eqv?", eqv)

             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("not=", numBoolBinop (/=))


             , ("string=?", strBoolBinop (==))

             -- Runtime type checks
             , ("string?", isString)
             , ("number?", isString)
             , ("symbol?", isString)

             -- Arithmetic
             , ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("/", numericBinop div)
             , ("*", numericBinop (*))]


apply :: String -> [Expr] -> ThrowsError Expr
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

eval :: Expr -> ThrowsError Expr
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Symbol _) = throwError $ Default "Environment not implemented yet..."
eval val@(Keyword _) = return val
eval Nil = return Nil
eval val@(Bool _) = return val

-- -------------
-- Special forms
-- -------------

-- Quote
eval (List [Symbol "quote", expr]) = return expr

-- If
eval (List [Symbol "if", pred, thenBranch, elseBranch]) = do
    result <- eval pred
    case result of
        Bool False -> eval elseBranch
        _ -> eval thenBranch

-- Cond
eval (List [Symbol "cond"]) = return $ Nil
eval (List ((Symbol "cond") : (List (pred : e : [])) : clauses)) = do
    result <- eval pred
    case result of
        Bool False -> eval $ List (Symbol "cond" : clauses)
        _ -> eval e
eval (List ((Symbol "cond") : (List val@(pred : e : _)) : _)) = throwError $ BadSpecialForm "Malformed cond" (List val)
eval cond@(List ((Symbol "cond") : _))  = throwError $ BadSpecialForm "Malformed cond" cond

-- Case
eval (List [Symbol "case", key]) = return $ Nil
eval (List ((Symbol "case") : key : (List (e1 : e2 : [])) : clauses)) = do
    evaledKey <- eval key
    evaledE1 <- eval e1
    if evaledKey == evaledE1
    then eval e2
    else eval (List ((Symbol "case") : evaledKey : clauses))
eval (List ((Symbol "case") : key : val@(List (e1 : e2 : _)) : clauses)) = throwError $ BadSpecialForm "Malformed case" val

-- And
eval (List [Symbol "and"]) = return $ Bool True
eval (List ((Symbol "and") : expr : [])) = eval expr
eval (List ((Symbol "and") : expr : exprs)) = do
    evaledExpr <- eval expr
    case evaledExpr of
        Bool False -> return $ Bool False
        _ -> return evaledExpr

-- Or
eval (List [Symbol "or"]) = return Nil
eval (List ((Symbol "or") : expr : [])) = eval expr
eval (List ((Symbol "or") : expr : exprs)) = do
    evaledExpr <- eval expr
    case evaledExpr of
        Bool True -> return evaledExpr
        _ -> eval (List ((Symbol "or") : exprs))

-- Function application
eval (List (Symbol fun : args)) = mapM eval args >>= apply fun

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

x = List [Symbol "quote", List [(Symbol "+"), Number 10, Number 32]]
y = List [ Symbol "if"
         , List [(Symbol ">"), Number 10, Number 2]
         , List [(Symbol "+"), Number 30, Number 2]
         , List [(Symbol "-"), Number 30, Number 30]
         ]
z = List [ Symbol "cons"
         , Number 42
         , List [ Symbol "quote"
                , List [Number 41, Number 40]
                ]
         ]

u = List [ Symbol "eq?"
         , Number 42
         , Number 42
         ]

v = List [ Symbol "cond"
         , List [ Bool True, Number 42]
         ]

v2 = List [ Symbol "cond"
          , List [ Bool False, Number 41]
          , List [ Bool False, Number 40]
          , List [ Bool True, Number 42]
          ]

v3 = List [ Symbol "case"
          , List [ Symbol "+"
                 , Number 10
                 , Number 1]
          , List [ Number 11
                 , Number 42
                 ]
          ]
x1 = List [ Symbol "+"
          , Number 32
          -- , Number 10
          -- , Number 2
          ]

