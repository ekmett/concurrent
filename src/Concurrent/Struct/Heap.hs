{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RoleAnnotations #-}
module Concurrent.Struct.Heap
  (
  -- * Mutable API
    Heap(..)
  , new
  , insert
  , extractMin
  , meld
  , delete
  , setKey
  -- * Transient API
  , Node(..)
  , insertNode
  , extractMinNode
  , meldNode
  , deleteNode
  , setKeyNode
  , key
  , value
  ) where

import Concurrent.Primitive.Class
import Concurrent.Struct.Ref
import Control.Monad
import Data.Struct.Internal
import Data.Struct.TH
import GHC.ST

makeStruct [d|
  data Node k v s = Node
    { parent, child, sibling :: !(Node k v s)
    , key :: k
    , value :: v
    } |]

newtype Heap k v s = Heap (Ref (Node k v) s)

type TransientHeap = Node

new :: MonadPrim s m => m (Heap k v s)
new = primST $ Heap <$> newRef Nil

modify :: MonadPrim s m => (TransientHeap k v s -> ST s (a, TransientHeap k v s)) -> Heap k v s -> m a
modify f (Heap h) = primST $ do
  r <- readRef h
  (a, r') <- f r
  writeRef h r'
  return a

modify_ :: MonadPrim s m => (TransientHeap k v s -> ST s (TransientHeap k v s)) -> Heap k v s -> m ()
modify_ f (Heap h) = primST $ do
  r <- readRef h
  r' <- f r
  writeRef h r'

insert :: (MonadPrim s m, Ord k) => k -> v -> Heap k v s -> m (Node k v s)
insert k v h = modify (insertNode k v) h

-- | @'insert' k v h@ returns the pair @(n,h')@ of the node for the inserted entry as well as the new heap
-- root.
insertNode :: Ord k => k -> v -> TransientHeap k v s -> ST s (Node k v s, TransientHeap k v s)
insertNode k v r
  | isNil r = do
    n <- newNode Nil Nil Nil k v
    return (n,n)
  | otherwise = do
    n <- newNode Nil Nil Nil k v
    r' <- meldNode r n
    return (n,r')

-- | Returns 'Nil' if there isn't a minimum node.
extractMin :: (MonadPrim s m, Ord k) => Heap k v s -> m (Node k v s)
extractMin = modify extractMinNode

extractMinNode :: Ord k => TransientHeap k v s -> ST s (Node k v s, TransientHeap k v s)
extractMinNode r
  | isNil r   = return (r, r)
  | otherwise = do
    r' <- combineSiblings r
    return (r, r')

meld :: (MonadPrim s m, Ord k) => Heap k v s -> Heap k v s -> m ()
meld (Heap p) (Heap q) = primST $ do
  a <- readRef p
  b <- readRef q
  c <- meldNode a b
  writeRef p c
  writeRef q Nil

-- | @'meldNode' l r@ merges the contents of two transient min-heaps @l@ and @r@
-- mutably. It destructively adds the contents of @r@ to @l@ and returns the modified @l@.
meldNode :: Ord k => TransientHeap k v s -> TransientHeap k v s -> ST s (TransientHeap k v s)
meldNode p q = do
  a <- getField key p
  b <- getField key q
  if a < b then p <$ attachChild p q
  else q <$ attachChild q p

attachChild :: Node k v s -> Node k v s -> ST s ()
attachChild p c = do
  s <- get child p
  set child p c
  set sibling c s
  set parent c p

combineSiblings :: Ord k => Node k v s -> ST s (Node k v s)
combineSiblings p = do
  pairChildren p
  linkChildren p

pairChildren :: Ord k => Node k v s -> ST s ()
pairChildren p = do
  lc <- get child p
  unless (isNil lc) $ do
    set child p Nil
    go lc
 where
  go c1 = do
    c2 <- get sibling c1
    if isNil c2 then attachChild p c1
    else do
      set sibling c1 Nil
      n <- get sibling c2
      set sibling c2 Nil
      c3 <- meldNode c1 c2
      attachChild p c3
      unless (isNil n) $ go n

linkChildren :: Ord k => Node k v s -> ST s (Node k v s)
linkChildren p = do
  r <- get child p
  if isNil r then return Nil
  else do
     c <- get sibling r
     set sibling r Nil
     go r c
 where
   go r c
     | isNil c   = return r
     | otherwise = do
       c' <- get sibling c
       set sibling c Nil
       r' <- meldNode r c
       go r' c'

delete :: (MonadPrim s m, Ord k) => Heap k v s -> Node k v s -> m ()
delete h n = modify_ (`deleteNode` n) h

-- | Remove a node from the heap it is in.
deleteNode :: Ord k => TransientHeap k v s -> Node k v s -> ST s (TransientHeap k v s)
deleteNode r n = do
  if r == n then snd <$> extractMinNode n
  else do
    p <- get parent n
    cutParent n
    q <- snd <$> extractMinNode n
    if isNil q then return r
    else meldNode p q

setKey :: (MonadPrim s m, Ord k) => Heap k v s -> Node k v s -> k -> m ()
setKey (Heap h) n k = primST $ do
  r <- readRef h
  r' <- setKeyNode r n k
  writeRef h r'

-- | Generalized 'decreaseKey' (allowed to increase key)
setKeyNode :: Ord k => TransientHeap k v s -> Node k v s -> k -> ST s (TransientHeap k v s)
setKeyNode h n k = do
  ok <- getField key n
  setField key n k
  if n == h && k <= ok then return h
  else do
   cutParent n
   meldNode h n

-- | Assumes parent exists
cutParent :: Ord k => Node k v s -> ST s ()
cutParent n = do
  p <- get parent n
  x <- get child p
  if x == n then do
    ns <- get sibling n
    set child p ns
  else deleteSibling n x
  set parent n Nil
  set sibling n Nil

deleteSibling :: Node k v s -> Node k v s -> ST s ()
deleteSibling n p = do
  q <- get sibling p
  if n == q then do
    ns <- get sibling n
    set sibling q ns
  else deleteSibling n q
