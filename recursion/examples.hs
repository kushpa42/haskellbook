factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go  n   d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)
