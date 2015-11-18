{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concurrent.Struct.Ref
  ( Ref
  , newRef
  , readRef
  , writeRef
  , casRef
  ) where

import Concurrent.Primitive.Class
import Control.Monad.Primitive
import Data.Struct.Internal
import GHC.Exts

data Ref (f :: * -> *) (s :: *) = Ref (MutVar# s Any)

instance Eq (Ref f s) where
  Ref p == Ref q = isTrue# (sameMutVar# p q)

newRef :: (MonadPrim s m, Struct f) => f s -> m (Ref f s)
newRef x = primitive $ \ s -> case unsafeCoerce# newMutVar# (destruct x) s of
  (# s', r #) -> (# s', Ref r #)

readRef :: (MonadPrim s m, Struct f) => Ref f s -> m (f s)
readRef (Ref r) = primitive $ \s -> case readMutVar# r s of
  (# s', y #) -> (# s', construct (unsafeCoerce# y) #)

writeRef :: (MonadPrim s m, Struct f) => Ref f s -> f s -> m ()
writeRef (Ref r) x = primitive_ $ \s -> unsafeCoerce# writeMutVar# r (destruct x) s

casRef :: (MonadPrim s m, Struct f) => Ref f s -> f s -> f s -> m (f s)
casRef (Ref r) x y = primitive $ \s -> case casMutVar## r (destruct x) (destruct y) s of
  (# s', _, z #) -> (# s', construct z #)

casMutVar## :: MutVar# s Any -> SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> (# State# s, Int#, SmallMutableArray# s Any #)
casMutVar## = unsafeCoerce# casMutVar#
