{-# LANGUAGE InstanceSigs #-}

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b)
        -> p a c
        -> p b c
  first f = bimap f id

  second :: (c -> d)
         -> p a c
         -> p a d
  second f = bimap id f

data Deux a b = Deux a b deriving (Show)

instance Bifunctor Deux where
  bimap :: (a -> b)
        -> (c -> d)
        -> Deux a c
        -> Deux b d
  bimap f g (Deux a c) = Deux (f a) (g c)

data Const a b = Const a deriving (Show)

instance Bifunctor Const where
  bimap :: (a -> b)
        -> (c -> d)
        -> Const a c
        -> Const b d
  bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c deriving (Show)

instance Bifunctor (Drei t) where
  bimap :: (a -> b)
        -> (c -> d)
        -> Drei t a c
        -> Drei t b d
  bimap f g (Drei t a c) = Drei t (f a) (g c)

data SuperDrei a b c = SuperDrei a b deriving (Show)

instance Bifunctor (SuperDrei t) where
  bimap :: (a -> b)
        -> (c -> d)
        -> SuperDrei t a c
        -> SuperDrei t b d
  bimap f _ (SuperDrei t a) = SuperDrei t (f a)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei t) where
  bimap :: (a -> b)
        -> (c -> d)
        -> SemiDrei t a c
        -> SemiDrei t b d
  bimap _ _ (SemiDrei t) = SemiDrei t

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps t l) where
  bimap :: (a -> b)
        -> (c -> d)
        -> Quadriceps t l a c
        -> Quadriceps t l b d
  bimap f g (Quadzzz t l a c) = Quadzzz t l (f a) (g c)

instance Bifunctor Either where
  bimap :: (a -> b)
        -> (c -> d)
        -> Either a c
        -> Either b d
  bimap f _ (Left a) = Left $ f a
  bimap _ g (Right c) = Right $ g c
