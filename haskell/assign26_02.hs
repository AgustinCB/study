{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad (liftM, liftM2)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT $ (fmap.fmap) f mma

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . (pure . Just)
  (MaybeT mmfab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> mmfab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT mma) >>= f = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just a -> runMaybeT $ f a

instance MonadTrans MaybeT where
  lift :: (Monad m) => m a -> MaybeT m a
  lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = lift . liftIO

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ \r -> fmap f (rma r)

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ \r -> pure a
  (ReaderT rmfab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmfab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    (runReaderT $ f a) r

instance MonadTrans (ReaderT r) where
  lift :: (Monad m) => m a -> ReaderT r m a
  lift ma = ReaderT $ const ma

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b)
       -> StateT s m a
       -> StateT s m b
  fmap f (StateT msa) = StateT $ \s ->
    fmap ((\b -> (b,s)).(f.fst)) (msa s)

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b)
        -> StateT s m a
        -> StateT s m b
  (StateT smfab) <*> (StateT sma) = StateT $ \s -> liftM2 (,)  (fa s) (pure s)
    where fa s = (f s) <*> (a s)
          a s = fmap fst $ sma s
          f s = fmap fst $ smfab s

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a
        -> (a -> StateT s m b)
        -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    as <- sma s
    runStateT (f $ fst as) s

instance MonadTrans (StateT s) where
  lift :: (Monad m) => m a -> StateT s m a
  lift ma = StateT $ \s -> liftM2 (,) ma (pure s)

instance (MonadIO m) => MonadIO (StateT r m) where
  liftIO :: IO a -> StateT r m a
  liftIO = lift . liftIO
