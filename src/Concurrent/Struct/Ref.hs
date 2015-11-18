{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concurrent.Struct.Ref
  ( StructRef
  , newStructRef
  , readStructRef
  , writeStructRef
  , casStructRef
  ) where

import Concurrent.Primitive.Class
import Control.Monad.Primitive
import Data.Struct.Internal
import GHC.Exts

data StructRef (f :: * -> *) (s :: *) = StructRef (MutVar# s Any)

instance Eq (StructRef f s) where
  StructRef p == StructRef q = isTrue# (sameMutVar# p q)

newStructRef :: (MonadPrim s m, Struct f) => f s -> m (StructRef f s)
newStructRef x = primitive $ \ s -> case unsafeCoerce# newMutVar# (destruct x) s of
  (# s', r #) -> (# s', StructRef r #)

readStructRef :: (MonadPrim s m, Struct f) => StructRef f s -> m (f s)
readStructRef (StructRef r) = primitive $ \s -> case readMutVar# r s of
  (# s', y #) -> (# s', construct (unsafeCoerce# y) #)

writeStructRef :: (MonadPrim s m, Struct f) => StructRef f s -> f s -> m ()
writeStructRef (StructRef r) x = primitive_ $ \s -> unsafeCoerce# writeMutVar# r (destruct x) s

casStructRef :: (MonadPrim s m, Struct f) => StructRef f s -> f s -> f s -> m (f s)
casStructRef (StructRef r) x y = primitive $ \s -> case casMutVar## r (destruct x) (destruct y) s of
  (# s', _, z #) -> (# s', construct z #)

casMutVar## :: MutVar# s Any -> SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> (# State# s, Int#, SmallMutableArray# s Any #)
casMutVar## = unsafeCoerce# casMutVar#
