{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-float-in #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Concurrent.Capability.Pinned
  ( Pinned(..)
  , runPinned
  , ReifiesCapability(..)
  ) where

import Concurrent.Thread
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Primitive
import Data.Tagged
import Unsafe.Coerce

-- This computation is pinned to a current thread.
newtype Pinned s a = Pinned { unpinned :: IO a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus,MonadThrow,MonadCatch,MonadMask)

instance PrimMonad (Pinned s) where
  type PrimState (Pinned s) = RealWorld
  primitive m = Pinned (primitive m)
  {-# INLINE primitive #-}

instance PrimBase (Pinned s) where
  internal (Pinned m) = internal m
  {-# INLINE internal #-}

class ReifiesCapability s where
  reflectCapability :: Tagged s Int

instance ReifiesCapability s => ReifiesCapability (Pinned s) where
  reflectCapability = retag (reflectCapability :: Tagged s Int)

reifyCapability :: forall r. (forall (s :: *). ReifiesCapability s => Pinned s r) -> Int -> IO r
reifyCapability k = unsafeCoerce (Magic k :: Magic r)

newtype Magic r = Magic (forall (s :: *). ReifiesCapability s => Pinned s r)

runPinned :: (forall (s :: *). ReifiesCapability s => Pinned s a) -> IO a
runPinned m = withCapability (currentCapability >>= reifyCapability m)
{-# INLINE runPinned #-}
