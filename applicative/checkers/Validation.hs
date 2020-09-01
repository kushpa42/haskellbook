import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a = Fail e
                    | Pass a
                    deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Fail e) = Fail e
  fmap f (Pass a) = Pass (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Pass 
  (<*>) (Fail e) (Fail e') = Fail (e <> e')
  (<*>) (Fail e) _            = Fail e
  (<*>) _           (Fail e)  = Fail e
  (<*>) (Pass f) (Pass a)  = Pass (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, Fail <$> arbitrary), (1, Pass <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

type V = Validation [String] (Int, String, String)

main :: IO ()
main = quickBatch $ applicative ((Pass (1 :: Int, "a", "b")) :: V)
