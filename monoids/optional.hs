data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada = Nada
  (<>) Nada a    = a
  (<>) a    Nada = a
  (<>) (Only a) (Only b) = Only $ (<>) a b
