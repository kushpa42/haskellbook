module Exercises where

import Test.QuickCheck
import Data.List (sort)
import Data.Char

half :: (Fractional a) => a -> a 
half x = x / 2

halfIdentity :: (Fractional a) => a -> a 
halfIdentity = (*2) . half

prop_half :: Double -> Bool
prop_half x = halfIdentity x == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

prop_orderedList :: [Int] -> Bool
prop_orderedList = listOrdered . sort

prop_plusAssociative :: Integer -> Integer -> Integer -> Bool
prop_plusAssociative x y z = (x + y) + z == x + (y + z)

prop_plusCommutative :: Integer -> Integer -> Bool
prop_plusCommutative x y = x + y == y + x

prop_multAssociative :: Integer -> Integer -> Integer -> Bool 
prop_multAssociative x y z = (x * y) * z == x * (y * z)

prop_multCommutative :: Integer -> Integer -> Bool
prop_multCommutative x y = x * y == y * x

prop_quotRem ::  Integer -> NonZero Integer -> Bool 
prop_quotRem x (NonZero y) = (quot x y) * y + (rem x y) == x

prop_divMod :: Integer -> NonZero Integer -> Bool 
prop_divMod x (NonZero y) = (div x y) * y + (mod x y) == x

prop_expoAssociative :: Integer -> Integer -> Integer -> Bool
prop_expoAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

prop_expoCommutative :: Integer -> Integer -> Bool
prop_expoCommutative x y = x ^ y == y ^ x

prop_reverseId :: (Eq a) => [a] -> Bool
prop_reverseId xs = (reverse $ reverse xs) == xs

prop_dolla :: Blind (Int -> Int) -> Int -> Bool
prop_dolla (Blind f) a = (f $ a) == f a

prop_compose :: Blind (Int -> Int) -> Blind (Int -> Int) -> Int -> Bool
prop_compose (Blind f) (Blind g) x = (f . g) x == f (g x)

prop_cons :: [Char] -> [Char] -> Bool
prop_cons xs ys = foldr (:) xs ys == (++) xs ys

prop_concat :: [String] -> Bool
prop_concat xss = foldr (++) [] xss == concat xss

prop_take :: Int -> [Char] -> Bool
prop_take n cs = length (take n cs) == n

prop_readShow :: Int -> Bool
prop_readShow x = read (show x) == x

square :: (Floating a) => a -> a
square x = x * x

prop_squareId :: Double -> Bool
prop_squareId x = (square . sqrt) x == x

twice :: (a -> a) -> (a -> a)
twice f = f . f


fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpper c : cs

prop_capitalizeIdem :: String -> Bool
prop_capitalizeIdem x = (capitalizeWord x == twice capitalizeWord x)
                     && (capitalizeWord x == fourTimes capitalizeWord x)

prop_sort :: [Char] -> Bool
prop_sort s = (sort s == twice sort s)
           && (sort s == fourTimes sort s)

main :: IO ()
main = do
    quickCheck prop_half
    quickCheck prop_orderedList
    quickCheck prop_plusAssociative
    quickCheck prop_plusCommutative
    quickCheck prop_multAssociative
    quickCheck prop_multCommutative
    quickCheck prop_quotRem
    quickCheck prop_divMod
    quickCheck prop_expoAssociative
    quickCheck prop_expoCommutative
    quickCheck (prop_reverseId :: [Int] -> Bool)
    quickCheck prop_dolla
    quickCheck prop_compose
    quickCheck prop_cons
    quickCheck prop_concat
    quickCheck prop_take
    quickCheck prop_readShow
    quickCheck prop_squareId
    quickCheck prop_capitalizeIdem
    quickCheck prop_sort

data Fool = Fulse | Frue deriving (Eq, Show)

-- equal probability
genFool :: Gen Fool
genFool = elements [Fulse, Frue]

-- diff probability
genFool' :: Gen Fool
genFool' = frequency [(2, return Fulse), (1, return Frue)]
