{-# LANGUAGE ExistentialQuantification #-}
module Evaluator where

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.HashMap as H

import Control.Monad
import Control.Monad.Error
import Parser
import Data.List (nub)
import Data.IORef
import System.IO (hPrint, stdout)

unpackNum :: Expr -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed
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
                             else do left <- unpacker $ head args
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
isString [String _] = return $ Bool True
isString _ = return $ Bool False

isSymbol :: [Expr] -> ThrowsError Expr
isSymbol [Symbol _] = return $ Bool True
isSymbol _ = return $ Bool False

isKeyword :: [Expr] -> ThrowsError Expr
isKeyword [Keyword _] = return $ Bool True
isKeyword _ = return $ Bool False

isNumber :: [Expr] -> ThrowsError Expr
isNumber [Number _] = return $ Bool True
isNumber _ = return $ Bool False

isSet :: [Expr] -> ThrowsError Expr
isSet [Set _] = return $ Bool True
isSet _ = return $ Bool False

isVector :: [Expr] -> ThrowsError Expr
isVector [Vector _] = return $ Bool True
isVector _ = return $ Bool False

isClosure :: [Expr] -> ThrowsError Expr
isClosure [PrimitiveFunc _ _] = return $ Bool True
isClosure [IOPrimitiveFunc _ _] = return $ Bool True
isClosure [Closure _ _ _ _] = return $ Bool True
isClosure _ = return $ Bool False

isHashMap :: [Expr] -> ThrowsError Expr
isHashMap [Hashmap _] = return $ Bool True
isHashMap _ = return $ Bool False


isNil :: [Expr] -> ThrowsError Expr
isNil (Nil:[]) = return $ Bool True
isNil [Symbol "quote", List []] = return $ Bool True
isNil _ = return $ Bool False

isList :: [Expr] -> ThrowsError Expr
isList es = do
  n <- isNil es
  case n of
    Bool True -> return $ Bool True
    _         -> isListAux es
      where isListAux [List _] = return $ Bool True
            isListAux [DottedList _ _] = return $ Bool True
            isListAux _ = return $ Bool False

cdr :: [Expr] -> ThrowsError Expr
cdr [List (_ : [])] = return $ Nil
cdr [List (_ : xs )] = return $ List xs
cdr [DottedList [] y] = return y
cdr [DottedList [_] y] = return y
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr badArg = throwError $ NumArgs 1 badArg

car :: [Expr] -> ThrowsError Expr
car [List (x : xs)] = return x
car [DottedList (x : _) _] = return x
car badArg = throwError $ NumArgs 1 badArg

cons :: [Expr] -> ThrowsError Expr
cons [expr, Nil] = return $ List [expr]
cons [expr, List exprs] = return $ List (expr : exprs)
cons [expr, DottedList es e] = return $ DottedList (expr : es) e
cons [expr, Vector e] = return $ Vector $ V.snoc e expr
-- TODO: Ord instance for Expr
-- cons [expr, Set e] = return $ Set $ S.insert expr e
cons badArg = throwError $ NumArgs 2 badArg

eqv :: [Expr] -> ThrowsError Expr
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [List arg1, List arg2]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

ioPrimitives :: [(String, [Expr] -> IOThrowsError Expr)]
ioPrimitives = [ ("slurp", readContents)
               , ("print", printAux)
               , ("println", printLn)
               , ("spit", writeProc)
               , ("apply", applyProc)]

readContents :: [Expr] -> IOThrowsError Expr
readContents [String filename] = liftM String $ liftIO $ readFile filename

printAux :: [Expr] -> IOThrowsError Expr
printAux [] = return Nil
printAux [x] = (liftIO $ putStr (show x)) >> return Nil
prinAux (x:xs) = (liftIO $ putStr (show x)) >> printLn xs

printLn :: [Expr] -> IOThrowsError Expr
printLn [] = (liftIO $ putChar '\n') >> return Nil
printLn [x] = (liftIO $ putStrLn (show x)) >> return Nil
prinLn (x:xs) = (liftIO $ putStr (show x)) >> printLn xs

writeProc :: [Expr] -> IOThrowsError Expr
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

applyProc :: [Expr] -> IOThrowsError Expr
applyProc [func, List args] = apply func args
applyProc [func, Vector args] = apply func (V.toList args)
applyProc [func, Set args] = apply func (S.toList args)
applyProc (func:args) = apply func args

load :: String -> IOThrowsError [Expr]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

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
             , ("string?",   isString)
             , ("number?",   isNumber)
             , ("symbol?",   isSymbol)
             , ("keyword?",  isKeyword)
             , ("fn?",       isClosure)

             , ("set?",      isSet)
             , ("hashmap?",  isHashMap)
             , ("vector?",   isVector)

             , ("list?",     isList)
             , ("seq?",      isList)
             , ("nil?",      isNil)

             -- Arithmetic
             , ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("/", numericBinop div)
             , ("*", numericBinop (*))]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives
                                               ++ map makePrimitiveIOFunc ioPrimitives)
    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc { name = var, fn = func })
          makePrimitiveIOFunc (var, func) = (var, IOPrimitiveFunc { name = var, iofn = func })

makeClosure :: (Maybe String) -> Env -> [Expr] -> [Expr] -> IOThrowsError Expr
makeClosure varargs env params body = return $ Closure (map show params) varargs body env
makeNormalClosure = makeClosure Nothing
makeVarArgsClosure = makeClosure . Just . show


eval :: Env -> Expr -> IOThrowsError Expr
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Keyword _) = return val
eval _ Nil = return Nil
eval _ val@(Bool _) = return val
eval env val@(Symbol var) = getVar env var
eval env (Vector xs) = mapM (eval env) xs >>= return . Vector
eval env (Set xs) = mapM (eval env) (S.elems xs) >>= return . Set . S.fromDistinctAscList . nub
-- eval env (Hashmap h) = mapM (\ (k, v) -> do key <- eval env k
--                                             val <- eval env v
--                                             return $ (key, val))
--                                             (H.toList h) >>= return . Hashmap . H.fromList . nub
-- TODO: Add an ORD typeclass instance to Expr

-- -------------
-- Special forms
-- -------------

-- Load
eval env (List [Symbol "load", String filename]) =
     load filename >>= liftM last . mapM (eval env)

-- Def
eval env (List [Symbol "def", (Symbol var), expr]) = eval env expr >>= defineVar env var

-- Defn
eval env (List ((Symbol "defn") : List (Symbol var : params) : body )) = makeNormalClosure env params body >>= defineVar env var
eval env (List ((Symbol "defn") : DottedList (Symbol var : params) varargs : body )) = makeVarArgsClosure varargs env params body >>= defineVar env var

-- Lambda
eval env (List (Symbol "lambda" : List params : body)) = makeNormalClosure env params body
eval env (List (Symbol "lambda" : DottedList params varargs : body)) = makeVarArgsClosure varargs env params body

-- Let
eval env (List (Symbol "let" : (List []) : body)) = liftM last $ mapM (eval env) body
eval envRef (List (Symbol "let" : (List ((List [Symbol x, e]) : bindings)) : body)) = do
  evaledE <- eval envRef e
  newEnv <- liftIO $ bindVars envRef [(x,evaledE)]
  inner <- eval newEnv (List (Symbol "let" :  List bindings : body))
  eval envRef $ List [ List (Symbol "lambda" : List [ Symbol x ] : inner : []), e ]
eval env badArg@(List (Symbol "let" : _)) = throwError $ BadSpecialForm "Malformed let" badArg

-- Quote
eval _ (List [Symbol "quote", expr]) = return expr
eval _ badArg@(List ((Symbol "quote") : _)) = throwError $ BadSpecialForm "Malformed quote" badArg

-- If
eval env (List [Symbol "if", pred, thenBranch, elseBranch]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env elseBranch
        _          -> eval env thenBranch
eval env badArg@(List ((Symbol "if") : _)) = throwError $ BadSpecialForm "Malformed if" badArg

-- Cond
-- TODO: Rewrite in terms of if
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
eval env (List (fun : args)) = do
    func <- eval env fun
    argVals <- mapM (eval env) args
    apply func argVals

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: Expr -> [Expr] -> IOThrowsError Expr
apply (IOPrimitiveFunc { name = name, iofn = func }) args = func args
apply (PrimitiveFunc {name = name, fn = func }) args = liftThrows $ func args
apply (Closure { params = params, vararg = vararg, body = body, env = env }) args =
        if num params /= num args && vararg == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars env $ zip params args) >>= bindVarArgs vararg >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing      -> return env

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

l = List [ Symbol "lambda"
         , List [ Symbol "x" ]
         , List [ Symbol "+"
                , Number 1
                , Symbol "x"
                ]
         ]

l1 = List [ Symbol "let"
         , List [ Symbol "x", Number 42 ]
         , List [ Symbol "+"
                , Number 1
                , Symbol "x"
                ]
         ]

inc = "(lambda (x) (+ x 1))"
factorial = "(defn (factorial n) (if (= n 1) 1 (* n (factorial (- n 1)))))"
mMap = "(defn (map f xs))"
n = List [Symbol "quote", List []]
