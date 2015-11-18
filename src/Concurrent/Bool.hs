module Concurrent.Bool where

import Concurrent.Promise
import Concurrent.Promise.Unsafe
import Concurrent.Par
import Control.Monad

infixr 3 <&&>

-- | Maximally lazy &&
(<&&>) :: Bool -> Bool -> Bool
x <&&> y = runPar $ do
  z <- newEmptyPromise
  fork $ if x then when y $ unsafeWritePromise z True
         else unsafeWritePromise z False
  fork $ unless y $ unsafeWritePromise z False
  return $ readPromise z

infixr 2 <||>

-- | Maximally lazy ||
(<||>) :: Bool -> Bool -> Bool
x <||> y = runPar $ do
  z <- newEmptyPromise
  fork $ if x then unsafeWritePromise z True
         else unless y $ unsafeWritePromise z False
  fork $ when y $ unsafeWritePromise z True
  return $ readPromise z
