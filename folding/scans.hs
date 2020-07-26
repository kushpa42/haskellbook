fibs    = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

twentyFibs = take 20 $ 1 : scanl (+) 1 twentyFibs

fibsLessThan x = takeWhile (< x) $ 1 : scanl (+) 1 fibs

facs    = 1 : scanl (*) 1 [2..]
facsN x = facs !! x
