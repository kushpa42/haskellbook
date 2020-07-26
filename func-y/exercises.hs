tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' x = d
  where (m, r) = x `divMod` 10
        (_, d) = m `divMod` 10

hunsD x = d
  where (m, r) = x `divMod` 100
        (_, d) = m `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y bool = case bool of
                      False -> x
                      True  -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y bool
  | bool == False = x
  | bool == True  = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
