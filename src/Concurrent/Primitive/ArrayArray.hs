{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Concurrent.Primitive.ArrayArray
  ( 
  -- * ArrayArrays
    ArrayArray(..)
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
  ) where

import Concurrent.Primitive.Class
import Concurrent.Primitive.SmallArray
import Control.Monad.Primitive
import Data.Primitive
import GHC.Types
import GHC.Prim

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

copyArrayArray :: MonadPrim s m => ArrayArray -> Int -> MutableArrayArray s -> Int -> Int -> m ()
copyArrayArray (ArrayArray a) (I# i) (MutableArrayArray m) (I# j) (I# k) = primitive_ $ \s ->
  copyArrayArray# a i m j k s
{-# INLINE copyArrayArray #-}

copyMutableArrayArray :: MonadPrim s m => MutableArrayArray s -> Int -> MutableArrayArray s -> Int -> Int -> m ()
copyMutableArrayArray (MutableArrayArray m) (I# i) (MutableArrayArray n) (I# j) (I# k) = primitive_ $ \s ->
  copyMutableArrayArray# m i n j k s
{-# INLINE copyMutableArrayArray #-}

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
