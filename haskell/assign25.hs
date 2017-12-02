{-# LANGUAGE InstanceSigs #-}

newtype Identity a =
  Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose.(pure.pure)

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ pure (<*>) <*> f <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> (Compose f g a) -> m
  foldMap f (Compose fga) = foldMap (\ga -> foldMap f ga) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative t => (a -> t b) -> Compose f g a -> t (Compose f g b)
  traverse f (Compose fga) = pure Compose <*> res
    where res = traverse (\ga -> traverse f ga) fga
