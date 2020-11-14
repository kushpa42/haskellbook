data E a b = L a
           | R b
           deriving (Eq, Ord, Show)

instance Functor (E a) where
  fmap _ (L x) = L x
  fmap f (R y) = R (f y)

instance Applicative (E a) where
  pure        = R
  (L e) <*> _ = L e
  (R f) <*> r = fmap f r

instance Foldable (E a) where
  foldMap _ (L _) = mempty
  foldMap f (R y) = f y

  foldr _ z (L _) = z
  foldr f z (R y) = f y z

instance Traversable (E a) where
  traverse _ (L x) = pure (L x)
  traverse f (R y) = R <$> f y
