{-# LANGUAGE CPP #-}
module Concurrent.Capability.LocalArray
  ( LocalArray
  , newLocalArray
  , readLocalArray
  , writeLocalArray
  , atomicModifyLocalArray
  , atomicModifyLocalArray'
  , modifyLocalArray
  , modifyLocalArray'
  , fetchModifyLocalArray
  , fetchModifyLocalArray'
  ) where

import Concurrent.Internal.Util
import Concurrent.Primitive
import Concurrent.Thread
import Control.Concurrent
import Control.Monad.Primitive
import Data.Primitive

-- | Capability-Local variables with cache-line spacing
--
-- You can _only_ safely access these from code that is pinned to a given capability. Otherwise you might get
-- preempted between when we check the capability # and index into the array. At which point the invariants
-- that ensure that we can operate without a compare-and-swap even on the threaded RTS cease to hold.

newtype LocalArray a = LocalArray SmallArrayArray

-- | Calling 'Control.Concurrent.setNumCapabilities' after this is built can cause you to crash when accessing it
-- and break invariants.
newLocalArray :: MonadPrimIO m => Int -> a -> m (LocalArray a)
newLocalArray n a = primIO $ do
  cs <- getNumCapabilities
  r <- newSmallArrayArray cs
  forN_ 0 cs $ \i -> do
    m <- newArray n a
    writeMutableArraySmallArray r i m
  LocalArray <$> unsafeFreezeSmallArrayArray r

-- | We could upgrade this to expand gracefully if we see an out of bounds capability.
getLocalArray :: MonadPrimIO m => LocalArray a -> m (MutableArray RealWorld a)
getLocalArray (LocalArray m) = indexMutableArraySmallArray m <$> currentCapability
{-# INLINE getLocalArray #-}

readLocalArray :: MonadPrimIO m => LocalArray a -> Int -> m a
readLocalArray l i = do
  arr <- getLocalArray l
  readArray arr i
{-# INLINE readLocalArray #-}

writeLocalArray :: MonadPrimIO m => LocalArray a -> Int -> a -> m ()
writeLocalArray l i a = do
  arr <- getLocalArray l
  writeArray arr i a
{-# INLINE writeLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
atomicModifyLocalArray :: MonadPrimIO m => LocalArray a -> Int -> (a -> (a, b)) -> m b
atomicModifyLocalArray l i f = do
  arr <- getLocalArray l
  localAtomicModifyArray arr i f
{-# INLINE atomicModifyLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
atomicModifyLocalArray' :: MonadPrimIO m => LocalArray a -> Int -> (a -> (a, b)) -> m b
atomicModifyLocalArray' l i f = do
  arr <- getLocalArray l
  localAtomicModifyArray' arr i f
{-# INLINE atomicModifyLocalArray' #-}

-- | This is safely atomic, despite the lack of CAS.
modifyLocalArray :: MonadPrimIO m => LocalArray a -> Int -> (a -> a) -> m a
modifyLocalArray l i f = do
  arr <- getLocalArray l
  localModifyArray arr i f
{-# INLINE modifyLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
modifyLocalArray' :: MonadPrimIO m => LocalArray a -> Int -> (a -> a) -> m a
modifyLocalArray' l i f = do
  arr <- getLocalArray l
  localModifyArray' arr i f
{-# INLINE modifyLocalArray' #-}

-- | This is safely atomic, despite the lack of CAS.
fetchModifyLocalArray :: MonadPrimIO m => LocalArray a -> Int -> (a -> a) -> m a
fetchModifyLocalArray l i f = do
  arr <- getLocalArray l
  localFetchModifyArray arr i f
{-# INLINE fetchModifyLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
fetchModifyLocalArray' :: MonadPrimIO m => LocalArray a -> Int -> (a -> a) -> m a
fetchModifyLocalArray' l i f = do
  arr <- getLocalArray l
  localFetchModifyArray' arr i f
{-# INLINE fetchModifyLocalArray' #-}
