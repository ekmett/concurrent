{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module Concurrent.Par.Unsafe where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import GHC.Prim (RealWorld)

data Determinism = Deterministic | NonDeterministic
data Idempotence = Idempotent | NonIdempotent

type role Par nominal nominal nominal representational

newtype Par (d :: Determinism) (i :: Idempotence) (s :: *) a = Par { unPar :: IO a }
  deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask)

instance (d ~ 'NonDeterministic, i ~ 'NonIdempotent, s ~ RealWorld) => MonadIO (Par d i s) where
  liftIO = Par

-- idempotent computations
class Monad m => MonadPar (d :: Determinism) (i :: Idempotence) (s :: *) m | m -> d i s where
  unsafeParIO :: IO a -> m a
  fork :: m a -> m ()

instance MonadPar d i s m => MonadPar d i s (ExceptT e m) where
  unsafeParIO m    = ExceptT $ Right <$> unsafeParIO m
  fork (ExceptT m) = ExceptT $ Right <$> fork m

instance MonadPar d i s m => MonadPar d i s (MaybeT m) where
  unsafeParIO m   = MaybeT $ Just <$> unsafeParIO m
  fork (MaybeT m) = MaybeT $ Just <$> fork m

instance MonadPar d i s m => MonadPar d i s (IdentityT m) where
  unsafeParIO m      = IdentityT $ unsafeParIO m
  fork (IdentityT m) = IdentityT $ fork m

instance MonadPar d i s m => MonadPar d i s (ReaderT e m) where
  unsafeParIO m    = ReaderT $ \_ -> unsafeParIO m
  fork (ReaderT f) = ReaderT $ \s -> fork (f s)

instance MonadPar d i s m => MonadPar d i s (Strict.StateT e m) where
  unsafeParIO m          = Strict.StateT $ \s -> (, s) <$> unsafeParIO m
  fork (Strict.StateT f) = Strict.StateT $ \s -> (, s) <$> fork (f s)

instance MonadPar d i s m => MonadPar d i s (Lazy.StateT e m) where
  unsafeParIO m        = Lazy.StateT $ \s -> (, s) <$> unsafeParIO m
  fork (Lazy.StateT f) = Lazy.StateT $ \s -> (, s) <$> fork (f s)

instance MonadPar 'NonDeterministic 'NonIdempotent RealWorld IO where
  unsafeParIO = id
  fork m = () <$ forkIO (() <$ m)

instance MonadPar d i s (Par d i s) where
  unsafeParIO = Par
  fork (Par m) = Par (() <$ forkIO (() <$ m))
