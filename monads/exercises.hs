import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type Threeple = (Integer, String, [Bool])

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

nopeTest :: IO ()
nopeTest = do
    let nope :: Nope Threeple
        nope = undefined
    quickBatch $ functor nope
    quickBatch $ applicative nope
    quickBatch $ monad nope 


-- Either
data FlippedEither b a = L a
                       | R b
                       deriving (Eq, Show)

instance Functor (FlippedEither b) where
  fmap f (L a)  = L (f a)
  fmap _ (R b) = R b

instance Applicative (FlippedEither b) where
  pure = L
  (<*>) (L f) (L a) = L (f a)
  (<*>) (R b) _     = R b
  (<*>) _ (R b)     = R b

instance Monad (FlippedEither b) where
  return = pure
  (>>=) (R b) _  = R b
  (>>=) (L a) f  = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (FlippedEither b a) where
  arbitrary = frequency [(1, R <$> arbitrary), (1, L <$> arbitrary)]

instance (Eq b, Eq a) => EqProp (FlippedEither b a) where
  (=-=) = eq

flippedEitherTest :: IO ()
flippedEitherTest = do
    let a :: FlippedEither String Threeple
        a = undefined
    quickBatch $ functor a 
    quickBatch $ applicative a 
    quickBatch $ monad a 

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

idTest :: IO ()
idTest = do
    let a :: Identity Threeple
        a = undefined
    quickBatch $ functor a 
    quickBatch $ applicative a 
    quickBatch $ monad a 

-- List
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

instance Monad List where
  return = pure
  (>>=) xs f = flatMap f xs

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

listTest :: IO ()
listTest = do
    let xs :: List Threeple
        xs = undefined
    quickBatch $ functor xs
    quickBatch $ applicative xs
    quickBatch $ monad xs

j :: Monad m => m (m a) -> m a
j = (=<<) id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f as bs = f <$> as <*> bs

ap :: Monad m => m a -> m (a -> b) -> m b
ap = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (\a b -> a:b) <$> (f x) <*> meh xs f

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id
