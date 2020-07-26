-- Example of partial application of a data contructor
data ThereYet = There Float Int Bool deriving (Eq, Show)

nope :: Float -> Int -> Bool -> ThereYet
nope = There

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False
