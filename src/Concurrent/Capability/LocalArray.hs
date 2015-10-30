{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concurrent.Capability.LocalArray
  ( 
  -- * Capability-Local Arrays
    LocalArray
  , newLocalArray
  , readLocalArray
  , writeLocalArray
  , atomicModifyLocalArray
  , atomicModifyLocalArray'
  , modifyLocalArray
  , modifyLocalArray'
  , fetchModifyLocalArray
  , fetchModifyLocalArray'
  -- * Small Capability-Local Arrays
  , SmallLocalArray
  , newSmallLocalArray
  , readSmallLocalArray
  , writeSmallLocalArray
  , atomicModifySmallLocalArray
  , atomicModifySmallLocalArray'
  , modifySmallLocalArray
  , modifySmallLocalArray'
  , fetchModifySmallLocalArray
  , fetchModifySmallLocalArray'
  ) where

import Concurrent.Capability.Pinned
import Concurrent.Internal.Util
import Concurrent.Primitive
import Control.Concurrent
import Control.Monad.Primitive
import Data.Primitive
import Data.Tagged

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
reflectLocalArray :: ReifiesCapability s => LocalArray a -> Tagged s (MutableArray RealWorld a)
reflectLocalArray (LocalArray m) = indexMutableArraySmallArray m <$> reflectCapability
{-# INLINE reflectLocalArray #-}

withLocalArray :: forall m a r. ReifiesCapability m => LocalArray a -> (MutableArray RealWorld a -> m r) -> m r
withLocalArray l f = f $ untag (reflectLocalArray l :: Tagged m (MutableArray RealWorld a))
{-# INLINE withLocalArray #-}

readLocalArray :: forall m a. (MonadPrimIO m, ReifiesCapability m) => LocalArray a -> Int -> m a
readLocalArray l i = withLocalArray l $ \arr -> readArray arr i
{-# INLINE readLocalArray #-}

writeLocalArray :: (MonadPrimIO m, ReifiesCapability m) => LocalArray a -> Int -> a -> m ()
writeLocalArray l i a = withLocalArray l $ \arr -> writeArray arr i a
{-# INLINE writeLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
atomicModifyLocalArray :: (MonadPrimIO m, ReifiesCapability m) => LocalArray a -> Int -> (a -> (a, b)) -> m b
atomicModifyLocalArray l i f = withLocalArray l $ \arr -> localAtomicModifyArray arr i f
{-# INLINE atomicModifyLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
atomicModifyLocalArray' :: (MonadPrimIO m, ReifiesCapability m) => LocalArray a -> Int -> (a -> (a, b)) -> m b
atomicModifyLocalArray' l i f = withLocalArray l $ \arr -> localAtomicModifyArray' arr i f
{-# INLINE atomicModifyLocalArray' #-}

-- | This is safely atomic, despite the lack of CAS.
modifyLocalArray :: (MonadPrimIO m, ReifiesCapability m) => LocalArray a -> Int -> (a -> a) -> m a
modifyLocalArray l i f = withLocalArray l $ \arr -> localModifyArray arr i f
{-# INLINE modifyLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
modifyLocalArray' :: (MonadPrimIO m, ReifiesCapability m) => LocalArray a -> Int -> (a -> a) -> m a
modifyLocalArray' l i f = withLocalArray l $ \arr -> localModifyArray' arr i f
{-# INLINE modifyLocalArray' #-}

-- | This is safely atomic, despite the lack of CAS.
fetchModifyLocalArray :: (MonadPrimIO m, ReifiesCapability m) => LocalArray a -> Int -> (a -> a) -> m a
fetchModifyLocalArray l i f = withLocalArray l $ \arr -> localFetchModifyArray arr i f
{-# INLINE fetchModifyLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
fetchModifyLocalArray' :: (MonadPrimIO m, ReifiesCapability m) => LocalArray a -> Int -> (a -> a) -> m a
fetchModifyLocalArray' l i f = withLocalArray l $ \arr -> localFetchModifyArray' arr i f
{-# INLINE fetchModifyLocalArray' #-}

newtype SmallLocalArray a = SmallLocalArray SmallArrayArray

-- | Calling 'Control.Concurrent.setNumCapabilities' after this is built can cause you to crash when accessing it
-- and break invariants.
newSmallLocalArray :: MonadPrimIO m => Int -> a -> m (SmallLocalArray a)
newSmallLocalArray n a = primIO $ do
  cs <- getNumCapabilities
  r <- newSmallArrayArray cs
  forN_ 0 cs $ \i -> do
    m <- newSmallArray n a
    writeSmallMutableArraySmallArray r i m
  SmallLocalArray <$> unsafeFreezeSmallArrayArray r

-- | We could upgrade this to expand gracefully if we see an out of bounds capability.
reflectSmallLocalArray :: ReifiesCapability s => SmallLocalArray a -> Tagged s (SmallMutableArray RealWorld a)
reflectSmallLocalArray (SmallLocalArray m) = indexSmallMutableArraySmallArray m <$> reflectCapability
{-# INLINE reflectSmallLocalArray #-}

withSmallLocalArray :: forall m a r. ReifiesCapability m => SmallLocalArray a -> (SmallMutableArray RealWorld a -> m r) -> m r
withSmallLocalArray l f = f $ untag (reflectSmallLocalArray l :: Tagged m (SmallMutableArray RealWorld a))
{-# INLINE withSmallLocalArray #-}

readSmallLocalArray :: forall m a. (MonadPrimIO m, ReifiesCapability m) => SmallLocalArray a -> Int -> m a
readSmallLocalArray l i = withSmallLocalArray l $ \arr -> readSmallArray arr i
{-# INLINE readSmallLocalArray #-}

writeSmallLocalArray :: (MonadPrimIO m, ReifiesCapability m) => SmallLocalArray a -> Int -> a -> m ()
writeSmallLocalArray l i a = withSmallLocalArray l $ \arr -> writeSmallArray arr i a
{-# INLINE writeSmallLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
atomicModifySmallLocalArray :: (MonadPrimIO m, ReifiesCapability m) => SmallLocalArray a -> Int -> (a -> (a, b)) -> m b
atomicModifySmallLocalArray l i f = withSmallLocalArray l $ \arr -> localAtomicModifySmallArray arr i f
{-# INLINE atomicModifySmallLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
atomicModifySmallLocalArray' :: (MonadPrimIO m, ReifiesCapability m) => SmallLocalArray a -> Int -> (a -> (a, b)) -> m b
atomicModifySmallLocalArray' l i f = withSmallLocalArray l $ \arr -> localAtomicModifySmallArray' arr i f
{-# INLINE atomicModifySmallLocalArray' #-}

-- | This is safely atomic, despite the lack of CAS.
modifySmallLocalArray :: (MonadPrimIO m, ReifiesCapability m) => SmallLocalArray a -> Int -> (a -> a) -> m a
modifySmallLocalArray l i f = withSmallLocalArray l $ \arr -> localModifySmallArray arr i f
{-# INLINE modifySmallLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
modifySmallLocalArray' :: (MonadPrimIO m, ReifiesCapability m) => SmallLocalArray a -> Int -> (a -> a) -> m a
modifySmallLocalArray' l i f = withSmallLocalArray l $ \arr -> localModifySmallArray' arr i f
{-# INLINE modifySmallLocalArray' #-}

-- | This is safely atomic, despite the lack of CAS.
fetchModifySmallLocalArray :: (MonadPrimIO m, ReifiesCapability m) => SmallLocalArray a -> Int -> (a -> a) -> m a
fetchModifySmallLocalArray l i f = withSmallLocalArray l $ \arr -> localFetchModifySmallArray arr i f
{-# INLINE fetchModifySmallLocalArray #-}

-- | This is safely atomic, despite the lack of CAS.
fetchModifySmallLocalArray' :: (MonadPrimIO m, ReifiesCapability m) => SmallLocalArray a -> Int -> (a -> a) -> m a
fetchModifySmallLocalArray' l i f = withSmallLocalArray l $ \arr -> localFetchModifySmallArray' arr i f
{-# INLINE fetchModifySmallLocalArray' #-}
