{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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
  ) where

import Control.Exception (evaluate)
import Concurrent.Primitive.Class
import Control.Monad.Primitive
import Data.Primitive
import GHC.Exts

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
