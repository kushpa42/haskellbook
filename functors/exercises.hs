{-# LANGUAGE FlexibleInstances #-}

data Sum b a = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

data Company a c b = DeepBlue a c
                   | Something b
                   deriving (Eq, Show)

instance Functor (Company a b) where
  fmap f (DeepBlue a c) = DeepBlue a c
  fmap f (Something b)  = Something (f b)

data More b a = L a b a
              | R b a b
              deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance
               | Desk a
               | Floor b

instance Functor (Quant a) where
  fmap f (Floor b) = Floor (f b)
  fmap _ (Desk a)  = Desk a
  fmap _ Finance   = Finance


data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a


newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K (f b)


data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)


data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)


data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)


data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)


data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)


data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats fa fa' fa'') = MoreGoats (fmap f fa) (fmap f fa') (fmap f fa'')


data TalkToMe a = Halt
                | Print a
                | Read (String -> a)


instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print a) = Print (f a)
  fmap f (Read g) = Read (fmap f g)
