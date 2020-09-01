module MonoidSemigroup where

import Data.Monoid
import Test.QuickCheck

-- Monoid / Semigroup exercises

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Monoid left identity
mli :: (Eq m, Monoid m) => m -> Bool
mli x = (mempty <> x) == x

-- Monoid right identity
mlr :: (Eq m, Monoid m) => m -> Bool
mlr x = (x <> mempty) == x

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivialId = Trivial -> Bool

-- Identity a

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity a') = Identity (a <> a')

instance Monoid a => Monoid (Identity a) where
  mempty  = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
      a <- arbitrary
      return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
type IdentityId = Identity String -> Bool

-- Two a b

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a')  (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty  = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Two a b)

type TwoAssoc = Two [Int] String -> Two [Int] String -> Two [Int] String -> Bool
type TwoId = Two [Int] String -> Bool

-- Three a b c
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

type ThreeAssoc = Three String String String
               -> Three String String String
               -> Three String String String
               -> Bool

-- Four a b c d
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

-- Monoid instances skipped because pattern is same as type Two a b

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

type FourAssoc = Four [Int] [Char] [String] [Bool] 
              -> Four [Int] [Char] [String] [Bool]
              -> Four [Int] [Char] [String] [Bool]
              -> Bool

-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj a) (BoolConj b) = BoolConj $ a && b

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
      a <- arbitrary
      return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjId = BoolConj -> Bool

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj a) (BoolDisj b) = BoolDisj $ a || b

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
      a <- arbitrary
      return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjId = BoolDisj -> Bool

-- Or
data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Snd a) _       = Snd a
  (<>) _       (Snd b) = Snd b
  (<>) _       (Fst b) = Fst b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (1, return $ Fst a)
                , (1, return $ Snd b) ] 

type OrAssoc = (Or Int String) -> (Or Int String) -> (Or Int String) -> Bool

-- Combine
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ \n -> (f n <> g n) 

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
      f <- arbitrary
      return (Combine f)

type CombineAssoc = Blind (Combine Int (Sum Int))
                 -> Blind (Combine Int (Sum Int))
                 -> Blind (Combine Int (Sum Int))
                 -> Int
                 -> Bool

type CombineId = Blind (Combine Int (Sum Int)) -> Int -> Bool

combineAssoc :: (Eq b, Semigroup b) => Blind (Combine a b) -> Blind (Combine a b) -> Blind (Combine a b) -> a -> Bool 
combineAssoc (Blind f) (Blind g) (Blind h) x = unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x

cli :: (Eq b, Monoid b) => Blind (Combine a b) -> a -> Bool
cli (Blind f) x = unCombine (mempty <> f) x == unCombine f x

cri :: (Eq b, Monoid b) => Blind (Combine a b) -> a -> Bool
cri (Blind f) x = unCombine (f <> mempty) x == unCombine f x

-- Comp
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp $ f . g

instance Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
      f <- arbitrary
      return $ Comp f

type CompAssoc = Blind (Comp Int) -> Blind (Comp Int) -> Blind (Comp Int) -> Int -> Bool
type CompId = Blind (Comp Int) -> Int -> Bool

compAssoc :: (Eq a) => Blind (Comp a) -> Blind (Comp a) -> Blind (Comp a) -> a -> Bool
compAssoc (Blind f) (Blind g) (Blind h) x = unComp (f <> (g <> h)) x == unComp ((f <> g) <> h) x

compli :: (Eq a) => Blind (Comp a) ->  a -> Bool
compli (Blind f) x = unComp (mempty <> f) x == unComp f x 

compri :: (Eq a) => Blind (Comp a) -> a -> Bool
compri (Blind f) x = unComp (f <> mempty) x == unComp f x 

-- Validation
data Validation a b = Fail a
                    | Pass b
                    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Pass a) _           = Pass a
  (<>) _           (Pass b) = Pass b
  (<>) (Fail a) (Fail b) = Fail (a <> b)

validTest :: IO ()
validTest = do
    let failure :: String
                -> Validation String Int
        failure = Fail
        success :: Int
                -> Validation String Int
        success = Pass
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem (\s -> let (a, x) = f s
                                        (b, y) = g x in
                                        (a <> b, y))


instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

-- Tests 
validateSemigroup :: IO ()
validateSemigroup = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
    quickCheck (combineAssoc :: CombineAssoc)
    quickCheck (compAssoc :: CompAssoc)

validateMonoid :: IO ()
validateMonoid = do
    quickCheck (mli :: TrivialId)
    quickCheck (mlr :: TrivialId)
    quickCheck (mli :: IdentityId)
    quickCheck (mlr :: IdentityId)
    quickCheck (mli :: TwoId)
    quickCheck (mlr :: TwoId)
    quickCheck (mli :: BoolConjId)
    quickCheck (mlr :: BoolConjId)
    quickCheck (mli :: BoolDisjId)
    quickCheck (mlr :: BoolDisjId)
    quickCheck (cli :: CombineId)
    quickCheck (cri :: CombineId)
    quickCheck (compli :: CompId)
    quickCheck (compri :: CompId)

testMem :: IO ()
testMem = do
    let f' = Mem $ \s -> ("hi", s + 1)
        rmzero = runMem mempty 0
        rmleft = runMem (mempty <> f') 0
        rmright = runMem (f' <> mempty) 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0

