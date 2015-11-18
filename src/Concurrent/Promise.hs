module Concurrent.Promise
  ( Promise
  , newEmptyPromise
  , newPromise
  , readPromise
  , writePromise
  ) where

import Concurrent.Exception
import Concurrent.Promise.Unsafe
import Concurrent.Par
import Concurrent.Par.Unsafe
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import GHC.IO

newEmptyPromise :: MonadPar d i s m => m (Promise s a)
newEmptyPromise = unsafeParIO $ do
  x <- newEmptyMVar
  Promise x <$> unsafeDupableInterleaveIO (readMVar x)

-- | Build a filled Promise out of a known value
newPromise :: a -> Promise s a
newPromise a = unsafeDupablePerformIO $ do
  x <- newMVar a
  return $ Promise x a

-- | Fulfilling the promise is tied to the region parameter, but reading the value is not.
readPromise :: Promise s a -> a
readPromise (Promise _ a) = a

-- | This would be valid under @SafeHaskell@ in the presence of sound 'Eq' instances.
writePromise :: (MonadPar d i s m, Eq a) => Promise s a -> a -> m ()
writePromise (Promise m _) a = unsafeParIO $ do
  a' <- evaluate a
  mask_ $ do
    t <- tryPutMVar m a'
    unless t $ do
      b <- readMVar m
      unless (a' == b) $ throwIO Contradiction
