module Arby where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
        a <- arbitrary
        b <- arbitrary
        return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
        a <- arbitrary
        b <- arbitrary
        elements [Left a, Right b]

-- equal probability
genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
        a <- arbitrary
        elements [Nothing, Just a]

-- What QuickCheck does so
-- you get more Just values
genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do
        a <- arbitrary
        frequency [(1, return Nothing), (3, return (Just a))]

-- frequency :: [(Int a, Gen a)] -> Gen a

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
        a <- arbitrary
        return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b = First a | Second b deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ First a, return $ Second b]

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [(1, return Nothing), (3, liftM Just arbitrary)]

main :: IO ()
main = do
        sample trivialGen

