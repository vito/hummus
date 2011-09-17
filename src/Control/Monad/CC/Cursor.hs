{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies  #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.CC.Cursor
-- Copyright   : (c) Dan Doel
-- License     : MIT
--
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (Generalized algebraic data types,
--                             Functional Dependencies)
--
-- Implements various cursor datatypes for iterating over collections
module Control.Monad.CC.Cursor (
        Cursor(..),
        Iterator,
        generator,
        iterator,
        current,
        next,
        open,
        update,
--        Walkable(..),
--        Zipper,
--        zipper,
--        previousDir,
--        currentTerm,
--        move
    ) where

import Prelude hiding (zip, mapM, mapM_)
import Control.Monad hiding (mapM, mapM_)
import Control.Monad.CC

import Data.Maybe
import Data.Foldable
import Data.Traversable hiding (traverse)

-- | A generalized type that represents a reified data structure traversal.
-- The other traversal data types in this module are special cases of this
-- general type. Cursor is parameterized by four types:
--
-- m : The monad in which the Cursor object is usable.
--
-- r : The result type, which will be stored in the cursor once the traversal
--     has been completed.
--
-- b : The type that the cursor expects to receive before moving on to the
--     next element in the traversal.
--
-- a : The element type to which the Cursor provides access at each step in
--     the traversal.
data Cursor m r b a where
   Current :: Monad m => a -> (b -> m (Cursor m r b a)) -> Cursor m r b a
   Done    :: Monad m => r -> Cursor m r b a

-- | A simple iterator, which provides a way to view each of the elements of
-- a data structure in order.
type Iterator m a = Cursor m () () a

-- | A function for making a cursor out of a free form generator, similar to
-- using 'yield' in Ruby or Python. For example:
--
-- > generator $ \yield -> do a <- yield 1 ; yield 2 ; b <- yield 3 ; return [a,b]
generator :: MonadDelimitedCont p s m => ((a -> m b) -> m r) -> m (Cursor m r b a)
generator f = reset (\p -> Done `liftM` f (yield p))
 where yield p a = shift p (\k -> return $ Current a (k . return))

-- A general cursor builder; takes the traversal function, a data structure, and
-- returns a corresponding cursor. Currently not exported, just used internally.
makeCursor :: (MonadDelimitedCont p s m) =>
                ((a -> m b) -> t -> m r) -> t -> m (Cursor m r b a)
makeCursor iter t = generator $ flip iter t

-- | Creates an Iterator that will yield each of the elements of a Foldable in
-- order.
iterator :: (Foldable t, MonadDelimitedCont p s m) => t a -> m (Iterator m a)
iterator = makeCursor mapM_

-- | Advances an Iterator to the next element (has no effect on a finished Iterator).
next :: Iterator m a -> m (Iterator m a)
next = update ()

-- | Extracts the current element from a cursor, if applicable.
current :: Cursor m r b a -> Maybe a
current (Done _)      = Nothing
current (Current a _) = Just a

-- | Begins an updating traversal over a Traversable structure. At each step,
-- the cursor will hold an element of type a, and providing an element of type
-- b will move on to the next step. When done, a new Traversable object holding
-- elements of type b will be available.
open :: (Traversable t, MonadDelimitedCont p s m) => t a -> m (Cursor m (t b) b a)
open = makeCursor mapM

-- | Provides an item to a Cursor, moving on to the next step in the traversal.
-- (has no effect on a finished Cursor).
update :: b -> Cursor m r b a -> m (Cursor m r b a)
update _ c@(Done _)    = return c
update b (Current _ k) = k b

-- Removing for now. This isn't remotely done, and I need to reread ccshan's
-- stuff on zippers and such before I can begin to get it right.
{-
class Direction d where
    nextD :: d -> d

class Direction d => Walkable t d | t -> d where
    walk :: Monad m => (d -> t -> m (Maybe t, d)) -> t -> m t

data ListDir = LLeft | LRight

instance Direction ListDir where
    nextD = id

instance Walkable [a] ListDir where
    walk tr ll = fromMaybe ll `liftM` traverse LRight ll
     where
     traverse d l = do (ml, d') <- tr d l
                       let l' = fromMaybe l ml
                       maybe ml Just `liftM` select l' d'
     select _        LLeft  = return Nothing
     select l@(x:xs) LRight = do l' <- liftM (x:) `liftM` traverse LRight xs
                                 maybe l' Just `liftM` traverse LLeft (fromMaybe l l')
     select []       LRight = maybe Nothing Just `liftM` traverse LLeft []

type Zipper m t d = Cursor m t (Maybe t, d) (d,t)

zipper :: (MonadDelimitedCont p s m, Walkable t d) => t -> m (Zipper m t d)
zipper = makeCursor $ walk . curry

previousDir :: Zipper m t d -> Maybe d
previousDir (Done _)          = Nothing
previousDir (Current (d,_) _) = Just d

currentTerm :: Zipper m t d -> t
currentTerm (Done t)          = t
currentTerm (Current (_,t) _) = t

move :: d -> Zipper m t d -> m (Zipper m t d)
move _ z@(Done _) = return z
move d (Current _ k) = k (Nothing, d)
-}
