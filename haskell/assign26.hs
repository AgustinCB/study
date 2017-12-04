{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Class
import Control.Monad (liftM)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b)
       -> EitherT e m a
       -> EitherT e m b
  fmap f (EitherT o) = EitherT $ (fmap.fmap) f o

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure (Right a)

  (<*>) :: EitherT e m (a -> b)
        -> EitherT e m a
        -> EitherT e m b
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  (EitherT mea) >>= faet = EitherT $ do
    ea <- mea
    case ea of
      Left e -> return $ Left e
      Right a -> runEitherT $ faet a

instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m a -> EitherT e m a
  lift = EitherT . liftM Right

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea
  where swapEither :: Either e a -> Either a e
        swapEither (Left e) = Right e
        swapEither (Right a) = Left a

eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT meab) = meab >>= \e ->
  case e of
    Left a -> f a
    Right b -> g b
