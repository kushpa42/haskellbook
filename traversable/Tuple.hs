data Tuple a b = Tuple a b deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)

instance Monoid a => Applicative (Tuple a) where
  pure = Tuple mempty
  (<*>) (Tuple u f) (Tuple v x) = Tuple (u <> v) (f x)

instance Foldable (Tuple a) where
  foldMap f (Tuple a b) = f b
  foldr f z (Tuple a b) = f b z

instance Traversable (Tuple a) where
  traverse f (Tuple a b) = (Tuple a) <$> f b
