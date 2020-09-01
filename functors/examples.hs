data WhoCares a = ItDoesnt
                | Matter a
                | WhatThisIsCalled
                deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

-- data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- BAD. Breaks law of function composition
-- instance Functor CountingBad where
--   fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)

data CountingGood a = Heisenberg Int a deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

-- Yo functors can stack
replaceWithP :: a -> Char
replaceWithP = const 'p'

lms :: [Maybe String]
lms = [Just "Ave", Just "woohoo", Nothing]

singleF :: [Char]
singleF = fmap replaceWithP lms 

doubleF :: [Maybe Char]
doubleF = (fmap . fmap) replaceWithP lms

tripleF :: [Maybe String]
tripleF = (fmap . fmap . fmap) replaceWithP lms

-- (.) :: (b -> c) -> (a -> b) -> a -> c
--
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y
--
-- (.) :: ((m -> n) -> f m -> f n) -> ((x -> y) -> g x -> g y) -> ...
-- 
--                                              (  a   ) -> (     b    )
--     :: ((m   -> n)   -> f m   -> f n)    -> ((x -> y) -> (g x -> g y)) -> ...
--
--     :: ((g x -> g y) -> f g x -> f g y)) -> ((x -> y) -> (g x -> g y)) -> f g x -> f g y
--
--
-- (fmap . fmap) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b) 

lmls :: [Maybe [String]]
lmls = [Just ["Ha", "Ha"], Nothing, Just []]

lmc :: [Maybe Char]
lmc = (fmap . fmap) replaceWithP lmls

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do
    putStr "replaceWith'   lms:    "
    print (replaceWithP' lms)

    putStr "liftedReplace  lms:    "
    print (liftedReplace lms)

    putStr "liftedReplace' lms:    "
    print (liftedReplace' lms)

    putStr "twiceLifted    lms:    "
    print (twiceLifted lms)

    putStr "twiceLifted'   lms:    "
    print (twiceLifted' lms)

    putStr "thriceLifted   lms:    "
    print (thriceLifted lms)

    putStr "thriceLifted'  lms:    "
    print (thriceLifted' lms)
