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

-- evalExpr :: [Expr] -> ThrowsError Expr
-- evalExpr (expr:_) = eval expr

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
primitives = [
             -- ("eval", evalExpr)

             -- List Primitives
               ("car", car)
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


eval :: Env -> Expr -> IOThrowsError Expr
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Keyword _) = return val
eval _ Nil = return Nil
eval _ val@(Bool _) = return val
eval env val@(Symbol var) = getVar env var

-- -------------
-- Special forms
-- -------------

-- Def
eval env (List [Symbol "def", (Symbol var), expr]) = eval env expr >>= defineVar env var

-- Quote
eval _ (List [Symbol "quote", expr]) = return expr

-- If
eval env (List [Symbol "if", pred, thenBranch, elseBranch]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env elseBranch
        _          -> eval env thenBranch

-- Cond
eval _ (List [Symbol "cond"]) = return $ Nil
eval env (List ((Symbol "cond") : (List (pred : e : [])) : clauses)) = do
    result <- eval env pred
    case result of
        Bool False -> eval env $ List (Symbol "cond" : clauses)
        _ -> eval env e
eval _ (List ((Symbol "cond") : (List val@(pred : e : _)) : _)) = throwError $ BadSpecialForm "Malformed cond" (List val)
eval _ cond@(List ((Symbol "cond") : _))  = throwError $ BadSpecialForm "Malformed cond" cond

-- Case
eval _ (List [Symbol "case", key]) = return $ Nil
eval env (List ((Symbol "case") : key : (List (e1 : e2 : [])) : clauses)) = do
    evaledKey <- eval env key
    evaledE1 <- eval env e1
    if evaledKey == evaledE1
    then eval env e2
    else eval env (List ((Symbol "case") : evaledKey : clauses))
eval _ (List ((Symbol "case") : key : val@(List (e1 : e2 : _)) : clauses)) = throwError $ BadSpecialForm "Malformed case" val

-- And
eval _ (List [Symbol "and"]) = return $ Bool True
eval env (List ((Symbol "and") : expr : [])) = eval env expr
eval env (List ((Symbol "and") : expr : exprs)) = do
    evaledExpr <- eval env expr
    case evaledExpr of
        Bool False -> return $ Bool False
        _          -> eval env (List ((Symbol "and") : exprs))

-- Or
eval _ (List [Symbol "or"]) = return Nil
eval env (List ((Symbol "or") : expr : [])) = eval env expr
eval env (List ((Symbol "or") : expr : exprs)) = do
    evaledExpr <- eval env expr
    case evaledExpr of
        Bool True -> return evaledExpr
        _ -> eval env (List ((Symbol "or") : exprs))

-- Function application
eval env (List (Symbol fun : args)) = mapM (eval env) args >>= liftThrows . apply fun


eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [Expr] -> ThrowsError Expr
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)


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

