{-# LANGUAGE FlexibleInstances #-} 

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows  = Cows  Int deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- newtypes have no runtime overhead
-- reuse representation of type it contains
--
-- can easily reuse typeclasses for type that newtype contains


newtype TipTuple = TipTuple (Int, String)

instance TooMany TipTuple where
  tooMany (TipTuple (n, s)) = tooMany n


newtype GoatPairs = GoatPairs (Int, Int)

instance TooMany GoatPairs where
  tooMany (GoatPairs (x, y)) = tooMany (x + y)

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany (x + y)

instance TooMany Double where
  tooMany n = n > 5

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x || tooMany y
