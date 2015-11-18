module Concurrent.Bool where

import Concurrent.Par
import Concurrent.Par.Unsafe
import Concurrent.Promise
import Concurrent.Promise.Unsafe
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Foldable (traverse_)
import System.IO.Unsafe
import System.Mem.Weak

infixr 3 <&&>

-- | Maximally lazy '&&'
(<&&>) :: Bool -> Bool -> Bool
x <&&> y = runPar $ do
  z <- newEmptyPromise
  t1 <- fork $ if x then when y $ unsafeWritePromise z True
         else unsafeWritePromise z False
  t2 <- fork $ unless y $ unsafeWritePromise z False
  unsafeParIO $ do
    wt1 <- mkWeakThreadId t1
    wt2 <- mkWeakThreadId t2
    unsafeInterleaveIO $ do
      r <- evaluate (readPromise z)
      deRefWeak wt1 >>= traverse_ killThread
      deRefWeak wt2 >>= traverse_ killThread
      return r

infixr 2 <||>

-- | Maximally lazy '||'
(<||>) :: Bool -> Bool -> Bool
x <||> y = runPar $ do
  z <- newEmptyPromise
  t1 <- fork $ if x then unsafeWritePromise z True
               else unless y $ unsafeWritePromise z False
  t2 <- fork $ when y $ unsafeWritePromise z True
  unsafeParIO $ do
    wt1 <- mkWeakThreadId t1
    wt2 <- mkWeakThreadId t2
    unsafeInterleaveIO $ do
      r <- evaluate (readPromise z)
      deRefWeak wt1 >>= traverse_ killThread
      deRefWeak wt2 >>= traverse_ killThread
      return r
