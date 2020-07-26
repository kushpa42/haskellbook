isPalindrome :: Eq a => [a] -> Bool
isPalindrome s = s == reverse s

absoluteValue :: (Num a, Ord a) => a -> a
absoluteValue x = if x > 0 then x else -x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f firstTup secondTup = ((snd firstTup, snd secondTup), (fst firstTup, fst secondTup))

-- Correcting Syntax
x = (+)

g xs = x w 1
  where w = length xs


identity a = a

first (a, b) = a
