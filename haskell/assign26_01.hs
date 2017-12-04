{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Class
import Control.Monad (liftM, liftM2)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

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
