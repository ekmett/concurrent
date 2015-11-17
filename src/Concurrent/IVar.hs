{-# LANGUAGE Unsafe #-}
module Concurrent.IVar where

import Concurrent.Exception
import Concurrent.Par
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import GHC.IO

-- | An 'IVar' is a promise that can be fulfilled multiple times, but must be always be fulfilled with the same answer.
data IVar s a
  = IVar {-# UNPACK #-} !(MVar a) a

newEmptyIVar :: MonadPar d i s m => m (IVar s a)
newEmptyIVar = unsafeParIO $ do
  x <- newEmptyMVar
  IVar x <$> unsafeDupableInterleaveIO (readMVar x) `catch` \BlockedIndefinitelyOnMVar -> throw BlockedIndefinitelyOnIVar

-- | Build a filled IVar out of a known value
newIVar :: a -> IVar s a
newIVar a = unsafeDupablePerformIO $ do
  x <- newMVar a
  return $ IVar x a

readIVar :: IVar s a -> a
readIVar (IVar _ a)     = a

writeIVar :: (MonadPar d i s m, Eq a) => IVar s a -> a -> m ()
writeIVar (IVar m _) a = unsafeParIO $ do
  a' <- evaluate a
  t <- tryPutMVar m a'
  unless t $ do
     b <- readMVar m
     unless (a' == b) $ throwIO Contradiction

-- | Like 'writeIVar' but assumes (without checking) that the write is always consistent.
unsafeWriteIVar :: MonadPar d i s m => IVar s a -> a -> m ()
unsafeWriteIVar (IVar m _) a = unsafeParIO $ do
  a' <- evaluate a
  () <$ tryPutMVar m a'
