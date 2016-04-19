import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.HUnit hiding (assert)

import Parser
import Evaluator
import Utils

import Control.Monad
import Control.Monad.Error

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.HashMap as H

-- test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1 + 3, 2))

-- HUnit -> Those tests are not necessary (see quickcheck below)
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

-- QuickCheck -> So much nicer!
prop_list_parsing = forAll (oneof [ genListOf genExprStr
                                  , genVectorOf genKeyword
                                  , genSetOf genKeyword
                                  ]) $
                            \lst -> case readExpr lst of
                                        Right x -> True
                                        _       -> False

makeIfClause :: Expr -> Expr -> Expr -> Expr
makeIfClause pred thenBranch elseBranch =
  List [Symbol "if", pred, thenBranch, elseBranch]

-- prop_eval_if = monadicIO $
--   forAll ((,,) <$> (suchThat genAtomExpr (/= (Bool False))) <*> genAtomExpr <*> genAtomExpr) $
--   \ (pred, thenBranch, elseBranch) -> do
--     env     <- nullEnv
--     evaledE <- runErrorT $ eval env (makeIfClause pred thenBranch elseBranch)
--     assert True
    -- case evaledE of
    --     Right e -> e == thenBranch
    --     _ -> False




-- prop_addition :: Integer -> Integer -> Bool
-- prop_addition x y = (eval (List [(Symbol "+"), (Number x), (Number y)])) == Right (Number (x + y))

-- prop_addition =
--     forAll (listOf1 (elements [0,1,2,3,4,5,6,7,8,9])) $
--     \xs -> ((eval (List ((Symbol "+") : (map Number xs)))) == Right (Number (sum xs)))



main :: IO ()
main = do
    -- quickCheck prop_addition
    runTestTT parsingTests
    quickCheck prop_list_parsing

-- main = quickCheck prop_revapp
-- main = putStrLn "Test suite not yet implemented"
