import Test.QuickCheck

import Parser
import Evaluator

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

prop_addition :: Integer -> Integer -> Bool
prop_addition x y = (eval (List [(Symbol "+"), (Number x), (Number y)])) == Right (Number (x + y))

-- prop_addition =
--     forAll (listOf1 (elements [0,1,2,3,4,5,6,7,8,9])) $
--     \xs -> ((eval (List ((Symbol "+") : (map Number xs)))) == Right (Number (sum xs)))

main :: IO ()
main = do
    quickCheck prop_addition
    quickCheck prop_revapp

-- main = quickCheck prop_revapp
-- main = putStrLn "Test suite not yet implemented"
