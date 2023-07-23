import Control.Applicative(liftA3)

stops  = "pbtdkg"
vowels = "aeiou"

generate :: [a] -> [b] -> [(a, b, a)]
generate xs ys = [(x, y, z) | x <- xs, y <- ys, z <- xs]

stopVowelStops = generate stops vowels

stopVowelStops2 = [(x, y, z) | (x, y, z) <- stopVowelStops, x == 'p'] 

nouns = ["dog", "car", "bicycle"]
verbs = ["barks", "vrooms", "pedals"]

nounVerbNouns = generate nouns verbs

seekritFunc :: Fractional a => String -> a
seekritFunc x = (sum . map (\_ -> 1) . concat $ words x) / (sum $ map (\_-> 1) (words x))

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
