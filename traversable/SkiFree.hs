module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance Applicative n => Functor (S n) where
  fmap f (S _ a) = S (pure $ f a) (f a)

instance Applicative n => Applicative (S n) where
  pure a = S (pure a) a
  (<*>) (S n f) (S n' a) = S (n <*> n') (f a)

instance Foldable (S n) where
  foldMap f (S _ a) = f a

instance Applicative n => Traversable (S n) where
  traverse f (S _ a) = pure <$> f a

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
         => Arbitrary (S n a) where
           arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , EqProp a)
         => EqProp (S n a) where
           (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

main :: IO ()
main = do
    let trigger :: S [] (Int, Int, [Int])
        trigger = undefined
    quickBatch (traversable trigger)

test = sample' (arbitrary :: Gen (S [] Int))
