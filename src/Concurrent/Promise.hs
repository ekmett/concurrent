{-# LANGUAGE Unsafe #-}
module Concurrent.Promise where

import Concurrent.Exception
import Concurrent.Par
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import GHC.IO

-- | An 'Promise' can be fulfilled multiple times, but must be always be fulfilled with the same answer.
data Promise s a
  = Promise {-# UNPACK #-} !(MVar a) a

newEmptyPromise :: MonadPar d i s m => m (Promise s a)
newEmptyPromise = unsafeParIO $ do
  x <- newEmptyMVar
  Promise x <$> unsafeDupableInterleaveIO (readMVar x)

-- | Build a filled Promise out of a known value
newPromise :: a -> Promise s a
newPromise a = unsafeDupablePerformIO $ do
  x <- newMVar a
  return $ Promise x a

readPromise :: Promise s a -> a
readPromise (Promise _ a)     = a

writePromise :: (MonadPar d i s m, Eq a) => Promise s a -> a -> m ()
writePromise (Promise m _) a = unsafeParIO $ do
  a' <- evaluate a
  t <- tryPutMVar m a'
  unless t $ do
     b <- readMVar m
     unless (a' == b) $ throwIO Contradiction

-- | Like 'writePromise' but assumes (without checking) that the write is always consistent.
unsafeWritePromise :: MonadPar d i s m => Promise s a -> a -> m ()
unsafeWritePromise (Promise m _) a = unsafeParIO $ do
  a' <- evaluate a
  () <$ tryPutMVar m a'
