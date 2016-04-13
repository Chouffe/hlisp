import Test.QuickCheck
import Test.HUnit

import Parser
import Evaluator

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.HashMap as H

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1 + 3, 2))

parsingTests =  test [ "List" ~: "(+ 1 2)" ~: readExpr "(+ 1 2)" ~=? Right (List [Symbol "+", Number 1, Number 2])
                     , "List" ~: "( +  1   2)" ~: readExpr "( +  1   2)" ~=? Right (List [Symbol "+", Number 1, Number 2])
                     , "List" ~: "(+ 1 2 )" ~: readExpr "(+ 1 2 )" ~=? Right (List [Symbol "+", Number 1, Number 2])

                     , "Vector" ~: "[1 2 3]" ~: readExpr "[1 2 3]" ~=? Right (Vector (V.fromList [Number 1, Number 2, Number 3]))
                     , "Vector" ~: "[ 1  2   3]" ~: readExpr "[ 1  2   3]" ~=? Right (Vector (V.fromList [Number 1, Number 2, Number 3]))
                     , "Vector" ~: "[1 2 3  ]" ~: readExpr "[1 2 3  ]" ~=? Right (Vector (V.fromList [Number 1, Number 2, Number 3]))

                     , "Set" ~: "#{1 2 3}" ~: readExpr "#{1 2 3}" ~=? Right (Set (S.fromDistinctAscList [Number 1, Number 2, Number 3]))
                     , "Set" ~: "#{ 1  2  3}" ~: readExpr "#{ 1  2  3  }" ~=? Right (Set (S.fromDistinctAscList [Number 1, Number 2, Number 3]))

                     ]

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

genChar :: Gen Char
genChar = elements "abcdefghijkpqrstuvwxyABCDEFGHIJKLMNOPQRSTUVWXYZ"

genSpecialChar :: Gen Char
genSpecialChar = elements "!#$%&|*+-/<=>?@^_~"

genKeyword :: Gen String
genKeyword = (listOf1 (oneof [genSpecialChar, genChar])) >>= return . ((++) ":")

genKeywordExpr :: Gen Expr
genKeywordExpr = genKeyword >>= return . Keyword . drop 1

genNumber :: Gen Integer
genNumber = choose (-100, 100)

genNumberExpr :: Gen Expr
genNumberExpr = genNumber >>= return . Number

-- genListExpr :: Gen Expr
-- genListExpr =
--     do


-- prop_list_spaces :: Bool
-- prop_list_spaces =
--     forAll orderedList $ \xs ->
--     readExpr "(" ++ ")"



-- prop_addition :: Integer -> Integer -> Bool
-- prop_addition x y = (eval (List [(Symbol "+"), (Number x), (Number y)])) == Right (Number (x + y))

-- prop_addition =
--     forAll (listOf1 (elements [0,1,2,3,4,5,6,7,8,9])) $
--     \xs -> ((eval (List ((Symbol "+") : (map Number xs)))) == Right (Number (sum xs)))



main :: IO ()
main = do
    -- quickCheck prop_addition
    runTestTT parsingTests
    quickCheck prop_revapp

-- main = quickCheck prop_revapp
-- main = putStrLn "Test suite not yet implemented"
