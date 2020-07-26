data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ nat) = 1 + natToInteger nat 

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0     = Nothing
  | otherwise = Just (go x)
  where go x = if x == 0 then Zero else Succ (go $ x - 1)
