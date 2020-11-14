module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a 

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq
  
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary


-- Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant a') = Constant (a <> a')

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary


-- Maybe
data Optional a = Nada
                | Yep a
                deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Applicative Optional where
  pure = Yep
  (<*>) Nada    _        = Nada
  (<*>) (Yep f) optional = fmap f optional

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = fmap Yep (f a)

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, pure Nada), (5, Yep <$> arbitrary)]


-- List
data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b 
fold f b (Cons x xs)  = f x (fold f b xs)

append' :: List a -> List a -> List a
append' Nil ys = ys
append' (Cons x xs) ys = Cons x (append' xs ys)

concat' :: List (List a) -> List a
concat' = fold append' Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f alist = concat' (fmap f alist)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) flist alist = flatMap (\f -> fmap f alist) flist

instance Foldable List where
  foldr = fold

instance Traversable List where
  traverse _ Nil            = pure Nil
  traverse f (Cons a alist) = fmap Cons (f a) <*> traverse f alist

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (5, Cons <$> arbitrary <*> arbitrary)]


-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- Pair

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty
  (<*>) (Pair a f) (Pair a' b) = Pair (a <> a') (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = (Pair a) <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq


-- Big
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Monoid a => Applicative (Big a) where
  pure x = Big mempty x x
  (<*>) (Big a f f') (Big a' b b') = Big (a <> a') (f b) (f' b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = (Big a) <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq


-- Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')
  
instance Monoid a => Applicative (Bigger a) where
  pure b = Bigger mempty b b b
  (<*>) (Bigger a f f' f'') (Bigger a' b b' b'') =  Bigger (a <> a') (f b) (f' b') (f'' b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = (Bigger a) <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq


-- Tree
data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldMap _ Empty               = mempty
  foldMap f (Leaf a)            = f a
  foldMap f (Node left a right) = foldMap f left <> f a <> foldMap f right

  foldr _ b Empty               = b
  foldr f b (Leaf a)            = f a b
  foldr f b (Node left a right) = foldr f acc left
    where acc = f a (foldr f b right)

instance Traversable Tree where
  traverse _ Empty               = pure Empty
  traverse f (Leaf a)            = Leaf <$> f a
  traverse f (Node left a right) = Node <$> traverse f left <*> f a <*> traverse f right

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency [(1, pure Empty), (4, Leaf <$> arbitrary), (5, Node <$> arbitrary <*> arbitrary <*> arbitrary)]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

type TI = Tree

main :: IO ()
main = do
    let trigger :: TI (Int, Int, [Int])
        trigger = undefined
    quickBatch (traversable trigger)
