{-# LANGUAGE CPP #-}
module Concurrent.Capability.LocalArray
  ( LocalArray
  , newLocalArray
  , readLocalArray
  , writeLocalArray
  ) where

import Concurrent.Primitive
import Concurrent.Thread
import Control.Concurrent
import Control.Monad.Primitive
import Data.Bits
import Data.Primitive
import Foreign.Storable as S

-- | Capability-Local variables with cache-line spacing
data LocalArray a = LocalArray {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !(MutableArray RealWorld a)

instance Eq (LocalArray a) where
  LocalArray _ _ x == LocalArray _ _ y = sameMutableArray x y

pointersPerCacheLine :: Int
pointersPerCacheLine
  | S.sizeOf (undefined :: Int) == 4 = 16
  | S.sizeOf (undefined :: Int) == 8 = 8
  | otherwise = error "unknown number of pointers per cache line"

logPointersPerCacheLine :: Int
logPointersPerCacheLine
  | S.sizeOf (undefined :: Int) == 4 = 4
  | S.sizeOf (undefined :: Int) == 8 = 3
  | otherwise = error "unknown number of pointers per cache line"

-- | Calling 'Control.Concurrent.setNumCapabilities' after this is built will cause you to crash when accessing it
-- and break invariants.
newLocalArray :: MonadPrimIO m => Int -> a -> m (LocalArray a)
newLocalArray n a = primIO $ do
  cs <- getNumCapabilities
  let up = unsafeShiftR (n + pointersPerCacheLine - 1) logPointersPerCacheLine
  LocalArray n up <$> newArray (up*(cs-1)+n) a

readLocalArray :: MonadPrimIO m => LocalArray a -> Int -> m a
readLocalArray (LocalArray _ o arr) i = do
  j <- currentCapability
  readArray arr ((o*j)+i)

writeLocalArray :: MonadPrimIO m => LocalArray a -> Int -> a -> m ()
writeLocalArray (LocalArray _ o arr) i a = do
  j <- currentCapability
  writeArray arr ((o*j)+i) a
