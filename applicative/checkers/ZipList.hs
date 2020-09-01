import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _   = Nil
take' _ Nil = Nil
take' n (Cons a ls) = Cons a (take' (n - 1) ls)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)

instance Applicative List where
  pure a = Cons a (pure a)
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) (Cons a as) = Cons (f a) (fs <*> as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (5, Cons <$> arbitrary <*> arbitrary)]


newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (pure a)
  (<*>) (ZipList' fs) (ZipList' as) = ZipList' (fs <*> as)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

repeat' :: a -> List a
repeat' = pure

main :: IO ()
main = quickBatch $ applicative (ZipList' $ Cons (1 :: Int, "a", "b") Nil)
