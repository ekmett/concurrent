{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Edward Kmett 2015
-- License     : BSD-style
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Portability : non-portable
--
-- Various sorts of primitive arrays
--
--------------------------------------------------------------------------------
module Concurrent.Primitive.Array
  (
  -- * Array Primitives
    sizeofArray
  , sizeofMutableArray
  , casArray
  , atomicModifyArray
  , atomicModifyArray'
  , modifyArray
  , modifyArray'
  , fetchModifyArray
  , fetchModifyArray'

  -- * Capability-local array manipulation primitives
  , localAtomicModifyArray
  , localAtomicModifyArray'
  , localModifyArray
  , localModifyArray'
  , localFetchModifyArray
  , localFetchModifyArray'

  -- * ByteArray Primitives

  , sizeOfByteArray
  , sizeOfMutableByteArray
  , casIntArray
  , atomicReadIntArray
  , atomicWriteIntArray
  , fetchAddIntArray
  , fetchSubIntArray
  , fetchAndIntArray
  , fetchNandIntArray
  , fetchOrIntArray
  , fetchXorIntArray

  -- * Prefetching
  , prefetchByteArray0, prefetchByteArray1, prefetchByteArray2, prefetchByteArray3
  , prefetchMutableByteArray0, prefetchMutableByteArray1, prefetchMutableByteArray2, prefetchMutableByteArray3
  , prefetchValue0, prefetchValue1, prefetchValue2, prefetchValue3

  -- * SmallArrays
  , SmallArray(..)
  , SmallMutableArray(..)
  , newSmallArray
  , readSmallArray
  , writeSmallArray
  , indexSmallArray
  , indexSmallArrayM
  , unsafeFreezeSmallArray
  , unsafeThawSmallArray
  , sameSmallMutableArray
  , copySmallArray
  , copySmallMutableArray
  , cloneSmallArray
  , cloneSmallMutableArray
  , casSmallArray
  , sizeOfSmallArray
  , sizeOfSmallMutableArray
  -- ** Atomic modification
  , atomicModifySmallArray
  , atomicModifySmallArray'
  , modifySmallArray
  , modifySmallArray'
  , fetchModifySmallArray
  , fetchModifySmallArray'
  , localAtomicModifySmallArray
  , localAtomicModifySmallArray'
  , localModifySmallArray
  , localModifySmallArray'
  , localFetchModifySmallArray
  , localFetchModifySmallArray'

  -- * ArrayArrays
  , ArrayArray(..)
  , unsafeFreezeArrayArray
  , unsafeThawArrayArray
  , sizeofArrayArray
  -- ** Indexing
  , indexArrayArray
  , indexArrayArrayArray
  , indexByteArrayArray
  , indexMutableArrayArray
  , indexMutableArrayArrayArray
  , indexMutableByteArrayArray
  , indexSmallArrayArray
  , indexSmallMutableArrayArray
  -- * MutableArrayArrays
  , MutableArrayArray(..)
  , newArrayArray
  , copyArrayArray
  , copyMutableArrayArray
  , cloneArrayArray
  , cloneMutableArrayArray
  , sizeofMutableArrayArray
  -- ** Reading
  , readArrayArray
  , readArrayArrayArray
  , readByteArrayArray
  , readMutableArrayArray
  , readMutableArrayArrayArray
  , readMutableByteArrayArray
  , readSmallArrayArray
  , readSmallMutableArrayArray
  -- ** Writing
  , writeArrayArray
  , writeArrayArrayArray
  , writeByteArrayArray
  , writeMutableArrayArray
  , writeMutableArrayArrayArray
  , writeMutableByteArrayArray
  , writeSmallArrayArray
  , writeSmallMutableArrayArray

  -- * SmallArrayArrays
  , SmallArrayArray(..)
  , unsafeFreezeSmallArrayArray
  , unsafeThawSmallArrayArray
  , sizeofSmallArrayArray
  -- ** Indexing
{-
  , indexArraySmallArray
  , indexArrayArraySmallArray
  , indexByteArraySmallArray
  , indexMutableArraySmallArray
  , indexMutableArrayArraySmallArray
  , indexMutableByteArraySmallArray
  , indexSmallArraySmallArray
  , indexSmallMutableArraySmallArray
-}
  -- * SmallMutableMutableArrayArrays
  , SmallMutableArrayArray(..)
  , newSmallArrayArray
  , copySmallArrayArray
  , copySmallMutableArrayArray
  , cloneSmallArrayArray
  , cloneSmallMutableArrayArray
  , sizeofSmallMutableArrayArray
  -- ** Reading
{-
  , readArraySmallArray
  , readArrayArraySmallArray
  , readByteArraySmallArray
  , readMutableArraySmallArray
  , readMutableArrayArraySmallArray
  , readMutableByteArraySmallArray
  , readSmallArraySmallArray
  , readSmallMutableArraySmallArray
-}
  -- ** Writing
{-
  , writeArraySmallArray
  , writeArrayArraySmallArray
  , writeByteArraySmallArray
  , writeMutableArraySmallArray
  , writeMutableArrayArraySmallArray
  , writeMutableByteArraySmallArray
  , writeSmallArraySmallArray
  , writeSmallMutableArraySmallArray
-}
  ) where

import Concurrent.Primitive.Class
import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Zip
import Data.Data
import Data.Foldable as Foldable
import Data.Primitive
import GHC.Exts
import GHC.ST

--------------------------------------------------------------------------------
-- * Array Primitives
--------------------------------------------------------------------------------

sizeofArray :: Array a -> Int
sizeofArray (Array m) = I# (sizeofArray# m)

sizeofMutableArray :: MutableArray s a -> Int
sizeofMutableArray (MutableArray m) = I# (sizeofMutableArray# m)

casArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> a -> m (Int, a)
casArray (MutableArray m) (I# i) x y = primitive $ \s -> case casArray# m i x y s of
  (# s', i', z #) -> (# s', (I# i', z) #)

foreign import prim "atomicModifyArrayzh" atomicModifyArray# :: MutableArray# s a -> Int# -> Any -> State# s -> (#State# s, Any #)

atomicModifyArray## :: MutableArray# s a -> Int# -> (a -> (a, b)) -> State# s -> (# State# s, b #)
atomicModifyArray## = unsafeCoerce# atomicModifyArray#

atomicModifyArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> (a, b)) -> m b
atomicModifyArray (MutableArray m) (I# i) f = primitive $ \s -> atomicModifyArray## m i f s

atomicModifyArray' :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> (a, b)) -> m b
atomicModifyArray' m i f = primST $ do
  b <- atomicModifyArray m i $ \a ->
    case f a of
      v@(a',_) -> a' `seq` v
  b `seq` return b

foreign import prim "localAtomicModifyArrayzh" localAtomicModifyArray# :: MutableArray# s a -> Int# -> Any -> State# s -> (#State# s, Any #)

localAtomicModifyArray## :: MutableArray# s a -> Int# -> (a -> (a, b)) -> State# s -> (# State# s, b #)
localAtomicModifyArray## = unsafeCoerce# localAtomicModifyArray#

-- | Modify the contents of a position in an array in a manner that at least can't be preempted another thread in the current capability.
localAtomicModifyArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> (a, b)) -> m b
localAtomicModifyArray (MutableArray m) (I# i) f = primitive $ \s -> localAtomicModifyArray## m i f s

-- | Modify the contents of a position in an array strictly in a manner that at least can't be preempted another thread in the current capability.
localAtomicModifyArray' :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> (a, b)) -> m b
localAtomicModifyArray' m i f = primST $ do
  b <- localAtomicModifyArray m i $ \a -> case f a of v@(a',_) -> a' `seq` v
  unsafePrimToPrim (evaluate b)

foreign import prim "modifyArrayzh" modifyArray# :: MutableArray# s a -> Int# -> Any -> State# s -> (#State# s, Any, Any #)

modifyArray## :: MutableArray# s a -> Int# -> (a -> a) -> State# s -> (# State# s, a, a #)
modifyArray## = unsafeCoerce# modifyArray#

-- |
-- Modify the contents of an array at a given position. Return the new result
--
-- @
-- 'modifyArray' m i f = 'atomicModifyArray' m i $ \a -> let b = f a in (b, b)
-- @
modifyArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> a) -> m a
modifyArray (MutableArray m) (I# i) f = primitive $ \s -> case modifyArray## m i f s of
  (# s', _, a #) -> (# s', a #)

-- | Modify the contents of an array at a given position strictly. Return the new result.
--
-- Can this be smarter? e.g. start it off already as a blackhole we appear to be evaluating, putting frames on the stack, etc.
-- That would avoid anybody ever getting and seeing the unevaluated closure.
modifyArray' :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> a) -> m a
modifyArray' m i f = primST $ do
  a <- modifyArray m i f
  unsafePrimToPrim (evaluate a)

-- | Modify the contents of an array at a given position. Return the old result.
fetchModifyArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> a) -> m a
fetchModifyArray (MutableArray m) (I# i) f = primitive $ \s -> case modifyArray## m i f s of
  (# s', a, _ #) -> (# s', a #)

-- | Modify the contents of an array at a given position strictly. Return the old result.
fetchModifyArray' :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> a) -> m a
fetchModifyArray' (MutableArray m) (I# i) f = primitive $ \s -> case modifyArray## m i f s of
  (# s', a,  b #) -> case seq# b s' of
     (# s'' , _ #) -> (# s'', a #)

foreign import prim "localModifyArrayzh" localModifyArray# :: MutableArray# s a -> Int# -> Any -> State# s -> (#State# s, Any, Any #)

localModifyArray## :: MutableArray# s a -> Int# -> (a -> a) -> State# s -> (# State# s, a, a #)
localModifyArray## = unsafeCoerce# localModifyArray#

-- |
-- Modify the contents of an array at a given position. Return the new result.
--
-- Logically,
--
-- @
-- 'localModifyArray' m i f = 'localAtomicModifyArray' m i $ \a -> let b = f a in (b, b)
-- @
--
-- but it is a bit more efficient.
localModifyArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> a) -> m a
localModifyArray (MutableArray m) (I# i) f = primitive $ \s -> case localModifyArray## m i f s of
  (# s', _, a #) -> (# s', a #)

-- | Modify the contents of an array at a given position strictly. Return the new result.
--
-- Can this be smarter? e.g. start it off already as a blackhole we appear to be evaluating, putting frames on the stack, etc.
-- That would avoid anybody ever getting and seeing the unevaluated closure.
localModifyArray' :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> a) -> m a
localModifyArray' m i f = primST $ do
  a <- localModifyArray m i f
  unsafePrimToPrim (evaluate a)

-- | Modify the contents of an array at a given position. Return the old result.
localFetchModifyArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> a) -> m a
localFetchModifyArray (MutableArray m) (I# i) f = primitive $ \s -> case localModifyArray## m i f s of
  (# s', a, _ #) -> (# s', a #)

-- | Modify the contents of an array at a given position strictly. Return the old result.
localFetchModifyArray' :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> a) -> m a
localFetchModifyArray' (MutableArray m) (I# i) f = primitive $ \s -> case localModifyArray## m i f s of
  (# s', a,  b #) -> case seq# b s' of
     (# s'' , _ #) -> (# s'', a #)

-- TODO: Small Array equivalents

-- foreign import prim "atomicModifySmallArrayzh" atomicModifySmallArray# :: SmallMutableArray# s a -> Int# -> Any -> State# s -> (#State# s, Any #)
-- atomicModifySmallArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> (a -> (a, b)) -> m b
-- atomicModifySmallArray (SmallMutableArray m) (I# i) f = primitive $ \s -> unsafeCoerce# atomicModifySmallArray# m i f s

--------------------------------------------------------------------------------
-- * ByteArray Primitives
--------------------------------------------------------------------------------

sizeOfByteArray :: ByteArray -> Int
sizeOfByteArray (ByteArray m) = I# (sizeofByteArray# m)

sizeOfMutableByteArray :: MutableByteArray s -> Int
sizeOfMutableByteArray (MutableByteArray m) = I# (sizeofMutableByteArray# m)

casIntArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Int -> Int -> m Int
casIntArray (MutableByteArray m) (I# i) (I# x) (I# y) = primitive $ \s -> case casIntArray# m i x y s of
  (# s', i' #) -> (# s', I# i' #)

atomicReadIntArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> m Int
atomicReadIntArray (MutableByteArray m) (I# i) = primitive $ \s -> case atomicReadIntArray# m i s of
  (# s', i' #) -> (# s', I# i' #)

atomicWriteIntArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Int -> m ()
atomicWriteIntArray (MutableByteArray m) (I# i) (I# j) = primitive_ $ \s -> atomicWriteIntArray# m i j s

fetchAddIntArray  :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchAddIntArray (MutableByteArray m) (I# i) (I# j) = primitive $ \s -> case fetchAddIntArray# m i j s of
  (# s', k #) -> (# s', I# k #)

fetchSubIntArray  :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchSubIntArray (MutableByteArray m) (I# i) (I# j) = primitive $ \s -> case fetchSubIntArray# m i j s of
  (# s', k #) -> (# s', I# k #)

fetchAndIntArray  :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchAndIntArray (MutableByteArray m) (I# i) (I# j) = primitive $ \s -> case fetchAndIntArray# m i j s of
  (# s', k #) -> (# s', I# k #)

fetchNandIntArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchNandIntArray (MutableByteArray m) (I# i) (I# j) = primitive $ \s -> case fetchNandIntArray# m i j s of
  (# s', k #) -> (# s', I# k #)

fetchOrIntArray   :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchOrIntArray (MutableByteArray m) (I# i) (I# j) = primitive $ \s -> case fetchOrIntArray# m i j s of
  (# s', k #) -> (# s', I# k #)

fetchXorIntArray  :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Int -> m Int
fetchXorIntArray (MutableByteArray m) (I# i) (I# j) = primitive $ \s -> case fetchXorIntArray# m i j s of
  (# s', k #) -> (# s', I# k #)

--------------------------------------------------------------------------------
-- * Prefetching
--------------------------------------------------------------------------------

prefetchByteArray0, prefetchByteArray1, prefetchByteArray2, prefetchByteArray3 :: PrimMonad m => ByteArray -> Int -> m ()
prefetchByteArray0 (ByteArray m) (I# i) = primitive_ $ \s -> prefetchByteArray0# m i s
prefetchByteArray1 (ByteArray m) (I# i) = primitive_ $ \s -> prefetchByteArray1# m i s
prefetchByteArray2 (ByteArray m) (I# i) = primitive_ $ \s -> prefetchByteArray2# m i s
prefetchByteArray3 (ByteArray m) (I# i) = primitive_ $ \s -> prefetchByteArray3# m i s

prefetchMutableByteArray0, prefetchMutableByteArray1, prefetchMutableByteArray2, prefetchMutableByteArray3 :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> m ()
prefetchMutableByteArray0 (MutableByteArray m) (I# i) = primitive_ $ \s -> prefetchMutableByteArray0# m i s
prefetchMutableByteArray1 (MutableByteArray m) (I# i) = primitive_ $ \s -> prefetchMutableByteArray1# m i s
prefetchMutableByteArray2 (MutableByteArray m) (I# i) = primitive_ $ \s -> prefetchMutableByteArray2# m i s
prefetchMutableByteArray3 (MutableByteArray m) (I# i) = primitive_ $ \s -> prefetchMutableByteArray3# m i s

prefetchValue0, prefetchValue1, prefetchValue2, prefetchValue3 :: PrimMonad m => a -> m ()
prefetchValue0 a = primitive_ $ \s -> prefetchValue0# a s
prefetchValue1 a = primitive_ $ \s -> prefetchValue1# a s
prefetchValue2 a = primitive_ $ \s -> prefetchValue2# a s
prefetchValue3 a = primitive_ $ \s -> prefetchValue3# a s

--------------------------------------------------------------------------------
-- * ArrayArrays
--------------------------------------------------------------------------------

data ArrayArray = ArrayArray ArrayArray#

sizeofArrayArray :: ArrayArray -> Int
sizeofArrayArray (ArrayArray m) = I# (sizeofArrayArray# m)
{-# INLINE sizeofArrayArray #-}

data MutableArrayArray s = MutableArrayArray (MutableArrayArray# s) 

sizeofMutableArrayArray :: MutableArrayArray s -> Int
sizeofMutableArrayArray (MutableArrayArray m) = I# (sizeofMutableArrayArray# m)
{-# INLINE sizeofMutableArrayArray #-}

instance Eq (MutableArrayArray s) where
  MutableArrayArray x == MutableArrayArray y = isTrue# (sameMutableArrayArray# x y)
  {-# INLINE (==) #-}

newArrayArray :: MonadPrim s m => Int -> m (MutableArrayArray s)
newArrayArray (I# i) = primitive $ \s -> case newArrayArray# i s of
  (# s', a #) -> (# s', MutableArrayArray a #) 
{-# INLINE newArrayArray #-}

unsafeFreezeArrayArray :: MonadPrim s m => MutableArrayArray s -> m ArrayArray
unsafeFreezeArrayArray (MutableArrayArray m) = primitive $ \s -> case unsafeFreezeArrayArray# m s of
  (# s', a #) -> (# s', ArrayArray a #)
{-# INLINE unsafeFreezeArrayArray #-}

unsafeThawArrayArray# :: ArrayArray# -> State# s -> (# State# s, MutableArrayArray# s #)
unsafeThawArrayArray# = unsafeCoerce# unsafeThawArray#
{-# INLINE unsafeThawArrayArray# #-}

unsafeThawArrayArray :: MonadPrim s m => ArrayArray -> m (MutableArrayArray s)
unsafeThawArrayArray (ArrayArray a) = primitive $ \s -> case unsafeThawArrayArray# a s of
  (# s', m #) -> (# s', MutableArrayArray m #)
{-# INLINE unsafeThawArrayArray #-}

copyArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> ArrayArray -> Int -> Int -> m ()
copyArrayArray (MutableArrayArray m) (I# i) (ArrayArray a) (I# j) (I# k) = primitive_ $ \s -> copyArrayArray# a j m i k s
{-# INLINE copyArrayArray #-}

copyMutableArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> MutableArrayArray s -> Int -> Int -> m ()
copyMutableArrayArray (MutableArrayArray m) (I# i) (MutableArrayArray n) (I# j) (I# k) = primitive_ $ \s -> copyMutableArrayArray# n j m i k s
{-# INLINE copyMutableArrayArray #-}

-- | Return a newly allocated ArrayArray with the specified subrange of the
-- provided ArrayArray. The provided ArrayArray should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneArrayArray :: ArrayArray -- ^ source array
           -> Int     -- ^ offset into destination array
           -> Int     -- ^ number of elements to copy
           -> ArrayArray
cloneArrayArray (ArrayArray arr#) (I# off#) (I# len#) = case unsafeCoerce# cloneArray# arr# off# len# of
  arr'# -> ArrayArray arr'#
{-# INLINE cloneArrayArray #-}

-- | Return a newly allocated MutableArrayArray. with the specified subrange of
-- the provided MutableArrayArray. The provided MutableArrayArray should contain the
-- full subrange specified by the two Ints, but this is not checked.
cloneMutableArrayArray :: PrimMonad m
        => MutableArrayArray (PrimState m) -- ^ source array
        -> Int                          -- ^ offset into destination array
        -> Int                          -- ^ number of elements to copy
        -> m (MutableArrayArray (PrimState m))
cloneMutableArrayArray (MutableArrayArray arr#) (I# off#) (I# len#) = primitive $ \s# -> case unsafeCoerce# cloneMutableArray# arr# off# len# s# of
  (# s'#, arr'# #) -> (# s'#, MutableArrayArray arr'# #)
{-# INLINE cloneMutableArrayArray #-}

indexArrayArray :: ArrayArray -> Int -> Array a
indexArrayArray (ArrayArray m) (I# i) = Array (unsafeCoerce# indexArrayArrayArray# m i)
{-# INLINE indexArrayArray #-}

indexArrayArrayArray :: ArrayArray -> Int -> ArrayArray
indexArrayArrayArray (ArrayArray m) (I# i) = ArrayArray (indexArrayArrayArray# m i)
{-# INLINE indexArrayArrayArray #-}

indexByteArrayArray :: ArrayArray -> Int -> ByteArray
indexByteArrayArray (ArrayArray m) (I# i) = ByteArray (indexByteArrayArray# m i)
{-# INLINE indexByteArrayArray #-}

indexMutableArrayArray :: ArrayArray -> Int -> MutableArray s a
indexMutableArrayArray (ArrayArray m) (I# i) = MutableArray (unsafeCoerce# indexArrayArrayArray# m i)
{-# INLINE indexMutableArrayArray #-}

indexMutableArrayArrayArray :: ArrayArray -> Int -> MutableArrayArray s
indexMutableArrayArrayArray (ArrayArray m) (I# i) = MutableArrayArray (unsafeCoerce# indexArrayArrayArray# m i)
{-# INLINE indexMutableArrayArrayArray #-}

indexMutableByteArrayArray :: ArrayArray -> Int -> MutableByteArray s
indexMutableByteArrayArray (ArrayArray m) (I# i) = MutableByteArray (unsafeCoerce# indexByteArrayArray# m i)
{-# INLINE indexMutableByteArrayArray #-}

indexSmallArrayArray :: ArrayArray -> Int -> SmallArray a
indexSmallArrayArray (ArrayArray m) (I# i) = SmallArray (unsafeCoerce# indexArrayArrayArray# m i)
{-# INLINE indexSmallArrayArray #-}

indexSmallMutableArrayArray :: ArrayArray -> Int -> SmallMutableArray s a
indexSmallMutableArrayArray (ArrayArray m) (I# i) = SmallMutableArray (unsafeCoerce# indexArrayArrayArray# m i)
{-# INLINE indexSmallMutableArrayArray #-}

readArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> m (Array a)
readArrayArray (MutableArrayArray m) (I# i) = primitive $ \s -> case unsafeCoerce# readArrayArrayArray# m i s of
  (# s', o #) -> (# s', Array o #) 
{-# INLINE readArrayArray #-}

readArrayArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> m ArrayArray
readArrayArrayArray (MutableArrayArray m) (I# i) = primitive $ \s -> case readArrayArrayArray# m i s of
  (# s', o #) -> (# s', ArrayArray o #) 
{-# INLINE readArrayArrayArray #-}

readByteArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> m ByteArray
readByteArrayArray (MutableArrayArray m) (I# i) = primitive $ \s -> case readByteArrayArray# m i s of
  (# s', o #) -> (# s', ByteArray o #) 
{-# INLINE readByteArrayArray #-}

readMutableArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> m (MutableArray s a)
readMutableArrayArray (MutableArrayArray m) (I# i) = primitive $ \s -> case unsafeCoerce# readMutableArrayArrayArray# m i s of
  (# s', o #) -> (# s', MutableArray o #) 
{-# INLINE readMutableArrayArray #-}

readMutableArrayArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> m (MutableArrayArray s)
readMutableArrayArrayArray (MutableArrayArray m) (I# i) = primitive $ \s -> case readMutableArrayArrayArray# m i s of
  (# s', o #) -> (# s', MutableArrayArray o #) 
{-# INLINE readMutableArrayArrayArray #-}

readMutableByteArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> m (MutableByteArray s)
readMutableByteArrayArray (MutableArrayArray m) (I# i) = primitive $ \s -> case readMutableByteArrayArray# m i s of
  (# s', o #) -> (# s', MutableByteArray o #) 
{-# INLINE readMutableByteArrayArray #-}

readSmallArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> m (SmallArray a)
readSmallArrayArray (MutableArrayArray m) (I# i) = primitive $ \s -> case unsafeCoerce# readArrayArrayArray# m i s of
  (# s', o #) -> (# s', SmallArray o #) 
{-# INLINE readSmallArrayArray #-}

readSmallMutableArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> m (SmallMutableArray s a)
readSmallMutableArrayArray (MutableArrayArray m) (I# i) = primitive $ \s -> case unsafeCoerce# readMutableArrayArrayArray# m i s of
  (# s', o #) -> (# s', SmallMutableArray o #) 
{-# INLINE readSmallMutableArrayArray #-}

writeArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> Array a -> m ()
writeArrayArray (MutableArrayArray m) (I# i) (Array o) = primitive_ $ \s -> unsafeCoerce# writeArrayArrayArray# m i o s
{-# INLINE writeArrayArray #-}

writeArrayArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> ArrayArray -> m ()
writeArrayArrayArray (MutableArrayArray m) (I# i) (ArrayArray o) = primitive_ $ \s -> writeArrayArrayArray# m i o s
{-# INLINE writeArrayArrayArray #-}

writeByteArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> ByteArray -> m ()
writeByteArrayArray (MutableArrayArray m) (I# i) (ByteArray o) = primitive_ $ \s -> writeByteArrayArray# m i o s
{-# INLINE writeByteArrayArray #-}

writeMutableArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> MutableArray s a -> m ()
writeMutableArrayArray (MutableArrayArray m) (I# i) (MutableArray o) = primitive_ $ \s -> unsafeCoerce# writeMutableArrayArrayArray# m i o s
{-# INLINE writeMutableArrayArray #-}

writeMutableArrayArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> MutableArrayArray s -> m ()
writeMutableArrayArrayArray (MutableArrayArray m) (I# i) (MutableArrayArray o) = primitive_ $ \s -> writeMutableArrayArrayArray# m i o s
{-# INLINE writeMutableArrayArrayArray #-}

writeMutableByteArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> MutableByteArray s -> m ()
writeMutableByteArrayArray (MutableArrayArray m) (I# i) (MutableByteArray o) = primitive_ $ \s -> writeMutableByteArrayArray# m i o s
{-# INLINE writeMutableByteArrayArray #-}

writeSmallArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> MutableArray s a -> m ()
writeSmallArrayArray (MutableArrayArray m) (I# i) (MutableArray o) = primitive_ $ \s -> unsafeCoerce# writeArrayArrayArray# m i o s
{-# INLINE writeSmallArrayArray #-}

writeSmallMutableArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> MutableArray s a -> m ()
writeSmallMutableArrayArray (MutableArrayArray m) (I# i) (MutableArray o) = primitive_ $ \s -> unsafeCoerce# writeMutableArrayArrayArray# m i o s
{-# INLINE writeSmallMutableArrayArray #-}

--------------------------------------------------------------------------------
-- * SmallArrays
--------------------------------------------------------------------------------

-- | Boxed arrays
data SmallArray a = SmallArray (SmallArray# a)

-- | Mutable boxed arrays associated with a primitive state token.
data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)

instance Eq (SmallMutableArray s a) where
  (==) = sameSmallMutableArray

#ifndef HLINT
type role SmallMutableArray nominal representational
#endif

-- | Create a new mutable array of the specified size and initialise all
-- elements with the given value.
newSmallArray :: PrimMonad m => Int -> a -> m (SmallMutableArray (PrimState m) a)
{-# INLINE newSmallArray #-}
newSmallArray (I# n#) x = primitive
   (\s# -> case newSmallArray# n# x s# of
             (# s'#, arr# #) -> (# s'#, SmallMutableArray arr# #))

-- | Read a value from the array at the given index.
readSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> m a
{-# INLINE readSmallArray #-}
readSmallArray (SmallMutableArray arr#) (I# i#) = primitive (readSmallArray# arr# i#)

-- | Write a value to the array at the given index.
writeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> a -> m ()
{-# INLINE writeSmallArray #-}
writeSmallArray (SmallMutableArray arr#) (I# i#) x = primitive_ (writeSmallArray# arr# i# x)

-- | Read a value from the immutable array at the given index.
indexSmallArray :: SmallArray a -> Int -> a
{-# INLINE indexSmallArray #-}
indexSmallArray (SmallArray arr#) (I# i#) = case indexSmallArray# arr# i# of (# x #) -> x

-- | Monadically read a value from the immutable array at the given index.
-- This allows us to be strict in the array while remaining lazy in the read
-- element which is very useful for collective operations. Suppose we want to
-- copy an array. We could do something like this:
--
-- > copy marr arr ... = do ...
-- >                        writeSmallArray marr i (indexSmallArray arr i) ...
-- >                        ...
--
-- But since primitive arrays are lazy, the calls to 'indexSmallArray' will not be
-- evaluated. Rather, @marr@ will be filled with thunks each of which would
-- retain a reference to @arr@. This is definitely not what we want!
--
-- With 'indexSmallArrayM', we can instead write
--
-- > copy marr arr ... = do ...
-- >                        x <- indexSmallArrayM arr i
-- >                        writeSmallArray marr i x
-- >                        ...
--
-- Now, indexing is executed immediately although the returned element is
-- still not evaluated.
--
indexSmallArrayM :: Monad m => SmallArray a -> Int -> m a
{-# INLINE indexSmallArrayM #-}
indexSmallArrayM (SmallArray arr#) (I# i#)
  = case indexSmallArray# arr# i# of (# x #) -> return x

-- | Convert a mutable array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> m (SmallArray a)
{-# INLINE unsafeFreezeSmallArray #-}
unsafeFreezeSmallArray (SmallMutableArray arr#)
  = primitive (\s# -> case unsafeFreezeSmallArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, SmallArray arr'# #))

-- | Convert an immutable array to an mutable one without copying. The
-- immutable array should not be used after the conversion.
unsafeThawSmallArray :: PrimMonad m => SmallArray a -> m (SmallMutableArray (PrimState m) a)
{-# INLINE unsafeThawSmallArray #-}
unsafeThawSmallArray (SmallArray arr#)
  = primitive (\s# -> case unsafeThawSmallArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, SmallMutableArray arr'# #))

-- | Check whether the two arrays refer to the same memory block.
sameSmallMutableArray :: SmallMutableArray s a -> SmallMutableArray s a -> Bool
{-# INLINE sameSmallMutableArray #-}
sameSmallMutableArray (SmallMutableArray arr#) (SmallMutableArray brr#)
  = isTrue# (sameSmallMutableArray# arr# brr#)

-- | Copy a slice of an immutable array to a mutable array.
copySmallArray :: PrimMonad m
          => SmallMutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallArray a                         -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copySmallArray #-}
copySmallArray (SmallMutableArray dst#) (I# doff#) (SmallArray src#) (I# soff#) (I# len#)
  = primitive_ (copySmallArray# src# soff# dst# doff# len#)

-- | Copy a slice of a mutable array to another array. The two arrays may
-- not be the same.
copySmallMutableArray :: PrimMonad m
          => SmallMutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallMutableArray (PrimState m) a    -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copySmallMutableArray #-}
copySmallMutableArray (SmallMutableArray dst#) (I# doff#)
                 (SmallMutableArray src#) (I# soff#) (I# len#)
  = primitive_ (copySmallMutableArray# src# soff# dst# doff# len#)

-- | Return a newly allocated SmallArray with the specified subrange of the
-- provided SmallArray. The provided SmallArray should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneSmallArray :: SmallArray a -- ^ source array
           -> Int     -- ^ offset into destination array
           -> Int     -- ^ number of elements to copy
           -> SmallArray a
cloneSmallArray (SmallArray arr#) (I# off#) (I# len#)
  = case cloneSmallArray# arr# off# len# of arr'# -> SmallArray arr'#
{-# INLINE cloneSmallArray #-}

-- | Return a newly allocated SmallMutableArray. with the specified subrange of
-- the provided SmallMutableArray. The provided SmallMutableArray should contain the
-- full subrange specified by the two Ints, but this is not checked.
cloneSmallMutableArray :: PrimMonad m
        => SmallMutableArray (PrimState m) a -- ^ source array
        -> Int                          -- ^ offset into destination array
        -> Int                          -- ^ number of elements to copy
        -> m (SmallMutableArray (PrimState m) a)
{-# INLINE cloneSmallMutableArray #-}
cloneSmallMutableArray (SmallMutableArray arr#) (I# off#) (I# len#) = primitive
   (\s# -> case cloneSmallMutableArray# arr# off# len# s# of
             (# s'#, arr'# #) -> (# s'#, SmallMutableArray arr'# #))

instance IsList (SmallArray a) where
  type Item (SmallArray a) = a
  toList = Foldable.toList
  fromListN n xs0 = runST $ do
    arr <- newSmallArray n undefined
    let go !_ []     = return ()
        go k (x:xs) = writeSmallArray arr k x >> go (k+1) xs
    go 0 xs0
    unsafeFreezeSmallArray arr
  fromList xs = fromListN (Prelude.length xs) xs

instance Functor SmallArray where
  fmap f !i = runST $ do
    let n = length i
    o <- newSmallArray n undefined
    let go !k
          | k == n = return ()
          | otherwise = do
            a <- indexSmallArrayM i k
            writeSmallArray o k (f a)
            go (k+1)
    go 0
    unsafeFreezeSmallArray o

instance Foldable SmallArray where
  foldr f z arr = go 0 where
    n = length arr
    go !k
      | k == n    = z
      | otherwise = f (indexSmallArray arr k) (go (k+1))

  foldl f z arr = go (length arr - 1) where
    go !k
      | k < 0 = z
      | otherwise = f (go (k-1)) (indexSmallArray arr k)

  foldr' f z arr = go 0 where
    n = length arr
    go !k
      | k == n    = z
      | r <- indexSmallArray arr k = r `seq` f r (go (k+1))

  foldl' f z arr = go (length arr - 1) where
    go !k
      | k < 0 = z
      | r <- indexSmallArray arr k = r `seq` f (go (k-1)) r

  null a = length a == 0

  length = sizeOfSmallArray
  {-# INLINE length #-}

sizeOfSmallArray :: SmallArray a -> Int
sizeOfSmallArray (SmallArray a) = I# (sizeofSmallArray# a)
{-# INLINE sizeOfSmallArray #-}

instance Traversable SmallArray where
  traverse f a = fromListN (length a) <$> traverse f (Foldable.toList a)

instance Applicative SmallArray where
  pure a = runST $ newSmallArray 1 a >>= unsafeFreezeSmallArray
  (m :: SmallArray (a -> b)) <*> (n :: SmallArray a) = runST $ do
      o <- newSmallArray (lm * ln) undefined
      outer o 0 0
    where
      lm = length m
      ln = length n
      outer :: SmallMutableArray s b -> Int -> Int -> ST s (SmallArray b)
      outer o !i p
        | i < lm = do
            f <- indexSmallArrayM m i
            inner o i 0 f p
        | otherwise = unsafeFreezeSmallArray o
      inner :: SmallMutableArray s b -> Int -> Int -> (a -> b) -> Int -> ST s (SmallArray b)
      inner o i !j f !p
        | j < ln = do
            x <- indexSmallArrayM n j
            writeSmallArray o p (f x)
            inner o i (j + 1) f (p + 1)
        | otherwise = outer o (i + 1) p

instance Monad SmallArray where
  return = pure
  (>>) = (*>)
  fail _ = empty
  m >>= f = foldMap f m

instance MonadZip SmallArray where
  mzipWith (f :: a -> b -> c) m n = runST $ do
    o <- newSmallArray l undefined
    go o 0
    where
      l = min (length m) (length n)
      go :: SmallMutableArray s c -> Int -> ST s (SmallArray c)
      go o !i
        | i < l = do
          a <- indexSmallArrayM m i
          b <- indexSmallArrayM n i
          writeSmallArray o i (f a b)
          go o (i + 1)
        | otherwise = unsafeFreezeSmallArray o
  munzip m = (fmap fst m, fmap snd m)

instance MonadPlus SmallArray where
  mzero = empty
  mplus = (<|>)

instance Alternative SmallArray where
  empty = runST $ newSmallArray 0 undefined >>= unsafeFreezeSmallArray
  m@(SmallArray pm) <|> n@(SmallArray pn) = runST $ case length m of
     lm@(I# ilm) -> case length n of
       ln@(I# iln) -> do
         o@(SmallMutableArray po) <- newSmallArray (lm + ln) undefined
         primitive_ $ \s -> case copySmallArray# pm 0# po 0# ilm s of
           s' -> copySmallArray# pn 0# po ilm iln s'
         unsafeFreezeSmallArray o

instance Monoid (SmallArray a) where
  mempty = empty
  mappend = (<|>)

instance Show a => Show (SmallArray a) where
  showsPrec d as = showParen (d > 10) $
    showString "fromList " . showsPrec 11 (Foldable.toList as)

instance Read a => Read (SmallArray a) where
  readsPrec d = readParen (d > 10) $ \s -> [(fromList m, u) | ("fromList", t) <- lex s, (m,u) <- readsPrec 11 t]

instance Ord a => Ord (SmallArray a) where
  compare as bs = compare (Foldable.toList as) (Foldable.toList bs)

instance Eq a => Eq (SmallArray a) where
  as == bs = Foldable.toList as == Foldable.toList bs

instance NFData a => NFData (SmallArray a) where
  rnf a0 = go a0 (length a0) 0 where
    go !a !n !i
      | i >= n = ()
      | otherwise = rnf (indexSmallArray a i) `seq` go a n (i+1)
  {-# INLINE rnf #-}

instance Data a => Data (SmallArray a) where
  gfoldl f z m   = z fromList `f` Foldable.toList m
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = smallArrayDataType
  dataCast1 f    = gcast1 f

fromListConstr :: Constr
fromListConstr = mkConstr smallArrayDataType "fromList" [] Prefix

smallArrayDataType :: DataType
smallArrayDataType = mkDataType "Concurrent.Primitive.SmallArray.SmallArray" [fromListConstr]

--------------------------------------------------------------------------------
-- * Small Mutable Array combinators
--------------------------------------------------------------------------------

sizeOfSmallMutableArray :: SmallMutableArray s a -> Int
sizeOfSmallMutableArray (SmallMutableArray a) = I# (sizeofSmallMutableArray# a)
{-# INLINE sizeOfSmallMutableArray #-}

-- | Perform an unsafe, machine-level atomic compare and swap on an element within an array.
casSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> a -> a -> m (Int, a)
casSmallArray (SmallMutableArray m) (I# i) a b = primitive $ \s -> case casSmallArray# m i a b s of
  (# s', j, c #) -> (# s', (I# j, c) #)


foreign import prim "atomicModifySmallArrayzh" atomicModifySmallArray# :: SmallMutableArray# s a -> Int# -> Any -> State# s -> (#State# s, Any #)

atomicModifySmallArray## :: SmallMutableArray# s a -> Int# -> (a -> (a, b)) -> State# s -> (# State# s, b #)
atomicModifySmallArray## = unsafeCoerce# atomicModifySmallArray#

atomicModifySmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> (a, b)) -> m b
atomicModifySmallArray (SmallMutableArray m) (I# i) f = primitive $ \s -> atomicModifySmallArray## m i f s

atomicModifySmallArray' :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> (a, b)) -> m b
atomicModifySmallArray' m i f = primST $ do
  b <- atomicModifySmallArray m i $ \a ->
    case f a of
      v@(a',_) -> a' `seq` v
  b `seq` return b

foreign import prim "localAtomicModifySmallArrayzh" localAtomicModifySmallArray# :: SmallMutableArray# s a -> Int# -> Any -> State# s -> (#State# s, Any #)

localAtomicModifySmallArray## :: SmallMutableArray# s a -> Int# -> (a -> (a, b)) -> State# s -> (# State# s, b #)
localAtomicModifySmallArray## = unsafeCoerce# localAtomicModifySmallArray#

-- | Modify the contents of a position in an array in a manner that at least can't be preempted another thread in the current capability.
localAtomicModifySmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> (a, b)) -> m b
localAtomicModifySmallArray (SmallMutableArray m) (I# i) f = primitive $ \s -> localAtomicModifySmallArray## m i f s

-- | Modify the contents of a position in an array strictly in a manner that at least can't be preempted another thread in the current capability.
localAtomicModifySmallArray' :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> (a, b)) -> m b
localAtomicModifySmallArray' m i f = primST $ do
  b <- localAtomicModifySmallArray m i $ \a -> case f a of v@(a',_) -> a' `seq` v
  unsafePrimToPrim (evaluate b)

foreign import prim "modifySmallArrayzh" modifySmallArray# :: SmallMutableArray# s a -> Int# -> Any -> State# s -> (#State# s, Any, Any #)

modifySmallArray## :: SmallMutableArray# s a -> Int# -> (a -> a) -> State# s -> (# State# s, a, a #)
modifySmallArray## = unsafeCoerce# modifySmallArray#

-- |
-- Modify the contents of an array at a given position. Return the new result
--
-- @
-- 'modifySmallArray' m i f = 'atomicModifySmallArray' m i $ \a -> let b = f a in (b, b)
-- @
modifySmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> a) -> m a
modifySmallArray (SmallMutableArray m) (I# i) f = primitive $ \s -> case modifySmallArray## m i f s of
  (# s', _, a #) -> (# s', a #)

-- | Modify the contents of an array at a given position strictly. Return the new result.
--
-- Can this be smarter? e.g. start it off already as a blackhole we appear to be evaluating, putting frames on the stack, etc.
-- That would avoid anybody ever getting and seeing the unevaluated closure.
modifySmallArray' :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> a) -> m a
modifySmallArray' m i f = primST $ do
  a <- modifySmallArray m i f
  unsafePrimToPrim (evaluate a)

-- | Modify the contents of an array at a given position. Return the old result.
fetchModifySmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> a) -> m a
fetchModifySmallArray (SmallMutableArray m) (I# i) f = primitive $ \s -> case modifySmallArray## m i f s of
  (# s', a, _ #) -> (# s', a #)

-- | Modify the contents of an array at a given position strictly. Return the old result.
fetchModifySmallArray' :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> a) -> m a
fetchModifySmallArray' (SmallMutableArray m) (I# i) f = primitive $ \s -> case modifySmallArray## m i f s of
  (# s', a,  b #) -> case seq# b s' of
     (# s'' , _ #) -> (# s'', a #)

foreign import prim "localModifySmallArrayzh" localModifySmallArray# :: SmallMutableArray# s a -> Int# -> Any -> State# s -> (#State# s, Any, Any #)

localModifySmallArray## :: SmallMutableArray# s a -> Int# -> (a -> a) -> State# s -> (# State# s, a, a #)
localModifySmallArray## = unsafeCoerce# localModifySmallArray#

-- |
-- Modify the contents of an array at a given position. Return the new result.
--
-- Logically,
--
-- @
-- 'localModifySmallArray' m i f = 'localAtomicModifySmallArray' m i $ \a -> let b = f a in (b, b)
-- @
--
-- but it is a bit more efficient.
localModifySmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> a) -> m a
localModifySmallArray (SmallMutableArray m) (I# i) f = primitive $ \s -> case localModifySmallArray## m i f s of
  (# s', _, a #) -> (# s', a #)

-- | Modify the contents of an array at a given position strictly. Return the new result.
--
-- Can this be smarter? e.g. start it off already as a blackhole we appear to be evaluating, putting frames on the stack, etc.
-- That would avoid anybody ever getting and seeing the unevaluated closure.
localModifySmallArray' :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> a) -> m a
localModifySmallArray' m i f = primST $ do
  a <- localModifySmallArray m i f
  unsafePrimToPrim (evaluate a)

-- | Modify the contents of an array at a given position. Return the old result.
localFetchModifySmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> a) -> m a
localFetchModifySmallArray (SmallMutableArray m) (I# i) f = primitive $ \s -> case localModifySmallArray## m i f s of
  (# s', a, _ #) -> (# s', a #)

-- | Modify the contents of an array at a given position strictly. Return the old result.
localFetchModifySmallArray' :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> (a -> a) -> m a
localFetchModifySmallArray' (SmallMutableArray m) (I# i) f = primitive $ \s -> case localModifySmallArray## m i f s of
  (# s', a,  b #) -> case seq# b s' of
     (# s'' , _ #) -> (# s'', a #)

data SmallArrayArray = SmallArrayArray (SmallArray# Any)

foreign import prim "newSmallArrayArrayzh" newSmallArrayArray# :: Int# -> State# s -> (# State# s, SmallMutableArray# s a #)

newSmallArrayArray :: PrimMonad m => Int -> m (SmallMutableArrayArray (PrimState m))
newSmallArrayArray (I# i) = primitive $ \s -> case newSmallArrayArray# i s of
  (# s', m #) -> (# s', SmallMutableArrayArray m #)

sizeofSmallArrayArray :: SmallArrayArray -> Int
sizeofSmallArrayArray (SmallArrayArray m) = I# (sizeofSmallArray# m) 

data SmallMutableArrayArray s = SmallMutableArrayArray (SmallMutableArray# s Any)

sizeofSmallMutableArrayArray :: SmallMutableArrayArray s -> Int
sizeofSmallMutableArrayArray (SmallMutableArrayArray m) = I# (sizeofSmallMutableArray# m) 

instance Eq (SmallMutableArrayArray s) where
  SmallMutableArrayArray m == SmallMutableArrayArray n = isTrue# (sameSmallMutableArray# m n)

unsafeFreezeSmallArrayArray :: PrimMonad m => SmallMutableArrayArray (PrimState m) -> m SmallArrayArray
unsafeFreezeSmallArrayArray (SmallMutableArrayArray m) = primitive $ \ s -> case unsafeFreezeSmallArray# m s of
  (# s', a #) -> (# s', SmallArrayArray a #)

unsafeThawSmallArrayArray :: PrimMonad m => SmallArrayArray -> m (SmallMutableArrayArray (PrimState m))
unsafeThawSmallArrayArray (SmallArrayArray a) = primitive $ \s -> case unsafeThawSmallArray# a s of
  (# s', m #) -> (# s', SmallMutableArrayArray m #)

-- | Copy a slice of an immutable array to a mutable array.
copySmallArrayArray :: PrimMonad m
          => SmallMutableArrayArray (PrimState m)    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallArrayArray                         -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copySmallArrayArray #-}
copySmallArrayArray (SmallMutableArrayArray dst#) (I# doff#) (SmallArrayArray src#) (I# soff#) (I# len#)
  = primitive_ (copySmallArray# src# soff# dst# doff# len#)

-- | Copy a slice of a mutable array to another array. The two arrays may
-- not be the same.
copySmallMutableArrayArray :: PrimMonad m
          => SmallMutableArrayArray (PrimState m)    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallMutableArrayArray (PrimState m)    -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copySmallMutableArrayArray #-}
copySmallMutableArrayArray (SmallMutableArrayArray dst#) (I# doff#) (SmallMutableArrayArray src#) (I# soff#) (I# len#)
  = primitive_ (copySmallMutableArray# src# soff# dst# doff# len#)

-- | Return a newly allocated SmallArray with the specified subrange of the
-- provided SmallArray. The provided SmallArray should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneSmallArrayArray :: SmallArrayArray -- ^ source array
           -> Int     -- ^ offset into destination array
           -> Int     -- ^ number of elements to copy
           -> SmallArrayArray
{-# INLINE cloneSmallArrayArray #-}
cloneSmallArrayArray (SmallArrayArray arr#) (I# off#) (I# len#) = case cloneSmallArray# arr# off# len# of
  arr'# -> SmallArrayArray arr'#

-- | Return a newly allocated SmallMutableArray. with the specified subrange of
-- the provided SmallMutableArray. The provided SmallMutableArray should contain the
-- full subrange specified by the two Ints, but this is not checked.
cloneSmallMutableArrayArray :: PrimMonad m
        => SmallMutableArrayArray (PrimState m) -- ^ source array
        -> Int                          -- ^ offset into destination array
        -> Int                          -- ^ number of elements to copy
        -> m (SmallMutableArrayArray (PrimState m))
cloneSmallMutableArrayArray (SmallMutableArrayArray arr#) (I# off#) (I# len#) = primitive $ \s# -> case cloneSmallMutableArray# arr# off# len# s# of
  (# s'#, arr'# #) -> (# s'#, SmallMutableArrayArray arr'# #)
{-# INLINE cloneSmallMutableArrayArray #-}

{-
  -- * SmallArrayArrays
  -- ** Indexing
  , indexArraySmallArray
  , indexArrayArraySmallArray
  , indexByteArraySmallArray
  , indexMutableArraySmallArray
  , indexMutableArrayArraySmallArray
  , indexMutableByteArraySmallArray
  , indexSmallArraySmallArray
  , indexSmallMutableArraySmallArray
  -- * MutableArraySmallArrays
  -- ** Reading
  , readArraySmallArray
  , readArrayArraySmallArray
  , readByteArraySmallArray
  , readMutableArraySmallArray
  , readMutableArrayArraySmallArray
  , readMutableByteArraySmallArray
  , readSmallArraySmallArray
  , readSmallMutableArraySmallArray
  -- ** Writing
  , writeArraySmallArray
  , writeArrayArraySmallArray
  , writeByteArraySmallArray
  , writeMutableArraySmallArray
  , writeMutableArrayArraySmallArray
  , writeMutableByteArraySmallArray
  , writeSmallArraySmallArray
  , writeSmallMutableArraySmallArray
-}
