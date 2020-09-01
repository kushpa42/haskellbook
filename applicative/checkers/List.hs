module List where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) flist alist = flatMap (\f -> fmap f alist) flist
                   -- fold (\g b ->  append (fmap g alist) b ) Nil flist

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b 
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (5, Cons <$> arbitrary <*> arbitrary)]

instance (Eq a) => EqProp (List a) where (=-=) = eq


main :: IO ()
main = quickBatch $ applicative (Cons (1 :: Int, "a", "b") Nil)
