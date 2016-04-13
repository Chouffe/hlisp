module Utils where

import Test.QuickCheck

import Parser
import Evaluator
import Data.List ( concat
                 , transpose
                 , nub
                 )

interleave :: [[a]] -> [a]
interleave = concat . transpose

-- Generators
genChar :: Gen Char
genChar = elements "abcdefghijkpqrstuvwxyABCDEFGHIJKLMNOPQRSTUVWXYZ"

genNil :: Gen String
genNil = elements ["nil", "'()"]

genBool :: Gen String
genBool = elements ["#t", "#f"]

genString :: Gen String
genString = do
    str <- listOf1 genChar
    return $ "\"" ++ str ++ "\""

genSymbol :: Gen String
genSymbol = do
    h  <- genChar
    tl <- listOf1 (oneof [genSpecialChar, genChar])
    return $ [h] ++ tl

genSpaces :: Gen String
genSpaces = do
    n <- choose (1, 10)
    return $ take n (repeat ' ')

genSpecialChar :: Gen Char
genSpecialChar = elements "!#$%&|*+-/<=>?@^_~"

genKeyword :: Gen String
genKeyword = fmap (":" ++) (listOf1 (oneof [genSpecialChar, genChar]))

genKeywordExpr :: Gen Expr
genKeywordExpr = fmap (Keyword . drop 1) genKeyword

genNumber :: Gen Integer
genNumber = choose (-100, 100)

genNumberExpr :: Gen Expr
genNumberExpr = fmap Number genNumber

genExprStr :: Gen String
genExprStr = oneof [ fmap show genNumber
                   , genKeyword
                   , genNil
                   , genString
                   , genBool
                   , genSymbol
                   ]

genContainerOf :: Bool -> String -> String -> Gen String -> Gen String
genContainerOf uniqExpr opening closing gen = do
    exprs <- listOf1 gen
    spaces <- vectorOf ((length exprs) + 1) genSpaces
    return $ opening ++ (foldl1 (++) $ interleave [spaces, if uniqExpr then nub exprs else exprs]) ++ closing

genListOf :: Gen String -> Gen String
genListOf = genContainerOf False "(" ")"

genVectorOf :: Gen String -> Gen String
genVectorOf = genContainerOf False "[" "]"

genSetOf :: Gen String -> Gen String
genSetOf = genContainerOf True "#{" "}"
