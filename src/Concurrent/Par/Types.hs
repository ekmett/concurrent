module Concurrent.Par.Types 
  ( Determinism(..)
  , Idempotence(..)
  ) where

data Determinism = Deterministic | NonDeterministic
data Idempotence = Idempotent | NonIdempotent
