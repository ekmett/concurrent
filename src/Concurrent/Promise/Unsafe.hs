{-# LANGUAGE Unsafe #-}
module Concurrent.Promise.Unsafe where

import Concurrent.Par
import Concurrent.Par.Unsafe
import Control.Concurrent.MVar
import Control.Exception

-- | An 'Promise' can be fulfilled multiple times, but must be always be fulfilled with the same answer.
data Promise s a = Promise {-# UNPACK #-} !(MVar a) a

-- | Like 'writePromise' but assumes (without checking) that the write is always consistent.
unsafeWritePromise :: MonadPar d i s m => Promise s a -> a -> m ()
unsafeWritePromise (Promise m _) a = unsafeParIO $ do
  a' <- evaluate a
  () <$ tryPutMVar m a'
