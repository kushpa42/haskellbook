module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go  n   d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- only works for whole numbers
multiply :: (Eq a, Num a) => a -> a -> a
multiply 0 _ = 0
multiply _ 0 = 0
multiply x 1 = x
multiply 1 y = y
multiply x y = x + multiply x (y - 1)

main :: IO ()
main = hspec $ do
        describe "Addition" $ do
                it "1 + 1 is greater than 1" $ do
                        (1 + 1) > 1 `shouldBe` True
                it "2 + 2 is equal to 4" $ do
                        2 + 2 `shouldBe` 4
                it "15 divided by 3 is 5" $ do
                        dividedBy 15 3 `shouldBe` (5, 0)
                it "22 divided by 5 is 4 remainder 2" $ do
                        dividedBy 22 5 `shouldBe` (4, 2)
                it "0 times 2 is 0" $ do
                        multiply 0 2 `shouldBe` 0
                it "5 times 10 is 50" $ do
                        multiply 5 10 `shouldBe` 50
                it "x + 1 is always greater than 1" $ do
                        property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
