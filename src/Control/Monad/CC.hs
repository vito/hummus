{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    UndecidableInstances, FunctionalDependencies, FlexibleInstances, GADTs #-}

--------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.CC
-- Copyright   : (c) R. Kent Dybvig, Simon L. Peyton Jones and Amr Sabry
-- License     : MIT
--
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (rank-2 types, multi-parameter type classes,
--                              functional dependencies)
--
-- A monadic treatment of delimited continuations.
--
--    Adapted from the paper
--      /A Monadic Framework for Delimited Continuations/,
--    by R. Kent Dybvig, Simon Peyton Jones and Amr Sabry
--      (<http://www.cs.indiana.edu/~sabry/papers/monadicDC.pdf>)
--
-- This module implements the delimited continuation monad and transformer,
-- using the sequence-of-frames implementation from the original paper.
module Control.Monad.CC (
        -- * The CC monad
        CC(..),
        runCC,
        -- * The CCT monad transformer
        CCT(..),
        runCCT,
        SubCont(),
        Prompt,
        MonadDelimitedCont(..),
        -- * Assorted useful control operators
        reset,
        shift,
        control,
        shift0,
        control0,
        abort,
        appk
        -- * Examples
        -- $Examples
    ) where

import Control.Applicative

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader

import Prelude hiding (catch)

import Control.Monad.CC.Seq
import Control.Monad.CC.Prompt

-- newtype Frame m ans a b = Frame (a -> CCT ans m b)
data Frame m ans a b = FFrame (a -> b)
                     | MFrame (a -> CCT ans m b)

type Cont ans m a = Seq (Frame m) ans a
newtype SubCont ans m a b = SC (SubSeq (Frame m) ans a b)

-- | The CCT monad transformer allows you to layer delimited control
-- effects over an arbitrary monad.
--
-- The CCT transformer is parameterized by the following types
--
-- * ans : A region parameter, so that prompts and subcontinuations
--         may only be used in the same region they are created.
--
-- * m   : the underlying monad
--
-- * a   : The contained value. A value of type CCT ans m a can be though
--         of as a computation that calls its continuation with a value of
--         type 'a'
newtype CCT ans m a = CCT { unCCT :: Cont ans m a -> P ans m ans }

instance (Monad m) => Functor (CCT ans m) where
    fmap f (CCT e) = CCT $ \k -> e (PushSeg (FFrame f) k)

instance (Monad m) => Applicative (CCT ans m) where
    pure  = return
    (<*>) = ap

instance (Monad m) => Monad (CCT ans m) where
    return v = CCT $ \k -> appk k v
    (CCT e1) >>= e2 = CCT $ \k -> e1 (PushSeg (MFrame e2) k)

instance MonadTrans (CCT ans) where
    lift m = CCT $ \k -> lift m >>= appk k

instance (MonadReader r m) => MonadReader r (CCT ans m) where
    ask = lift ask
    local f m = CCT $ \k -> local f (unCCT m k)

instance (MonadState s m) => MonadState s (CCT ans m) where
    get = lift get
    put = lift . put

instance (MonadIO m) => MonadIO (CCT ans m) where
    liftIO = lift . liftIO


-- Applies a continuation to a value. 
appk :: Monad m => Cont ans m a -> a -> P ans m ans
appk EmptyS        a = return a
appk (PushP _ k)   a = appk k a
appk (PushSeg f k) a = appFrame f a k
 where
 appFrame (MFrame g) b l = unCCT (g b) l
 appFrame (FFrame g) b l = appk l (g b)

-- | Executes a CCT computation, yielding a value in the underlying monad
runCCT :: (Monad m) => (forall ans. CCT ans m a) -> m a
runCCT c = runP (unCCT c EmptyS)

-- | The CC monad may be used to execute computations with delimited control.
newtype CC ans a = CC { unCC :: CCT ans Identity a }
    deriving (Functor, Monad, Applicative, 
                MonadDelimitedCont (Prompt ans) (SubCont ans Identity))

-- | Executes a CC computation, yielding a resulting value.
runCC  :: (forall ans. CC ans a) -> a
runCC c = runIdentity (runCCT (unCC c))

-- | A typeclass for monads that support delimited control operators.
-- The type varibles represent the following:
--
-- m : The monad itself
--
-- p : The associated type of prompts that may delimit computations in the monad
--
-- s : The associated type of sub-continuations that may be captured
class (Monad m) => MonadDelimitedCont p s m | m -> p s where
    -- | Creates a new, unique prompt.
    newPrompt   :: m (p a)
    -- | Delimits a computation with a given prompt.
    pushPrompt  :: p a -> m a -> m a
    -- | Abortively capture the sub-continuation delimited by the given
    -- prompt, and call the given function with it. The prompt does not appear
    -- delimiting the sub-continuation, nor the resulting computation.
    withSubCont :: p b -> (s a b -> m b) -> m a
    -- | Pushes a sub-continuation, reinstating it as part of the continuation.
    pushSubCont :: s a b -> m a -> m b
    -- | Checks if the given prompt is valid.
    isValidPrompt :: p b -> m Bool

instance (Monad m) => MonadDelimitedCont (Prompt ans) (SubCont ans m) (CCT ans m) where
    newPrompt = CCT $ \k -> newPromptName >>= appk k
    pushPrompt p (CCT e) = CCT $ \k -> e (PushP p k)
    withSubCont p f = CCT $ \k -> let (subk, k') = splitSeq p k
                                   in unCCT (f (SC subk)) k'
    pushSubCont (SC subk) (CCT e) = CCT $ \k -> e (pushSeq subk k)
    isValidPrompt p = CCT $ \k -> appk k (inSeq p k)

-- | An approximation of the traditional /reset/ operator. Creates a new prompt,
-- calls the given function with it, and delimits the resulting computation
-- with said prompt.
reset :: (MonadDelimitedCont p s m) => (p a -> m a) -> m a
reset e = newPrompt >>= \p -> pushPrompt p (e p)

-- -----
-- These originally had types like:
--
-- ((a -> m b) -> m b) -> m a
--
-- but I came to the conclusion that it would be convenient to be able to pass
-- in monadically typed values.
-- As a specific example, this makes the difference between
--
-- > shift q (\f -> f (dref p))
--
-- and
--
-- > join $ shift q (\f -> f (dref p))
--
-- In other words, one can expressed in terms of the other (I think), but
-- the fact that one has to insert a 'join' /outside/ the shift, and not
-- anywhere near where the sub-continuation is actually used is rather
-- odd, and difficult to remember compared to the difference between:
--
-- > shift q (\f -> f (return pureValue))
--
-- and
--
-- > shift q (\f -> f pureValue)
-- -----

-- | The traditional /shift/ counterpart to the above 'reset'. Reifies the
-- subcontinuation into a function, keeping both the subcontinuation, and
-- the resulting computation delimited by the given prompt.
shift :: (MonadDelimitedCont p s m) => p b -> ((m a -> m b) -> m b) -> m a
shift p f = withSubCont p $ \sk -> pushPrompt p $
                                f (\a -> pushPrompt p $ pushSubCont sk a)

-- | The /control/ operator, traditionally the counterpart of /prompt/. It does
-- not delimit the reified subcontinuation, so control effects therein can
-- escape. The corresponding prompt is performed equally well by 'reset' above.
control :: (MonadDelimitedCont p s m) => p b -> ((m a -> m b) -> m b) -> m a
control p f = withSubCont p $ \sk -> pushPrompt p $
                                f (\a -> pushSubCont sk a)

-- | Abortively captures the current subcontinuation, delimiting it in a reified
-- function. The resulting computation, however, is undelimited.
shift0 :: (MonadDelimitedCont p s m) => p b -> ((m a -> m b) -> m b) -> m a
shift0 p f = withSubCont p $ \sk -> f (\a -> pushPrompt p $ pushSubCont sk a)

-- | Abortively captures the current subcontinuation, delimiting neither it nor
-- the resulting computation.
control0 :: (MonadDelimitedCont p s m) => p b -> ((m a -> m b) -> m b) -> m a
control0 p f = withSubCont p $ \sk -> f (\a -> pushSubCont sk a)

-- | Aborts the current continuation up to the given prompt.
abort :: (MonadDelimitedCont p s m) => p b -> m b -> m a
abort p e = withSubCont p (\_ -> e)

-------------------------------------------------------------------------------
-- $Examples
--
-- This module provides many different control operators, so hopefully the
-- examples herein can help in selecting the right ones. The most raw are the
-- four contained in the 'MonadDelimitedCont' type class. The first, of course,
-- is 'newPrompt', which should be straight forward enough. Next comes
-- 'pushPromp't, which is the basic operation that delimits a computation.
-- In the absense of other control operators, it's simply a no-op, so
--
-- > pushPrompt p (return v) == return v
--
-- 'withSubCont' is the primitive that allows the capture of sub-continuations.
-- Unlike callCC, 'withSubCont' aborts the delimited continuation it captures,
-- so:
--
-- > pushPrompt p ((1:) `liftM` (2:) `liftM` withSubCont p (\k -> return []))
--
-- will yield a value of [] on running, not [1, 2].
--
-- The final primitive control operator is 'pushSubCont', which allows the use
-- of the sub-continuations captured using 'withSubCont'. So:
--
-- > pushPrompt p ((1:) `liftM1 (2:) `liftM`
-- >                 withSubCont p (\k -> pushSubCont k (return [])))
--
-- will yield the answer [1, 2]. /However/, Capturing a sub-continuation and
-- immediately pusshing it /is not/ a no-op, because the sub-continuation
-- does not contain the delimiting prompt (and, of course, pushSubCont does
-- not re-instate it, as it doesn't know what prompt was originally used).
-- Thus, capturing and pushing a sub-continuation results in the net loss of
-- one delimiter, and said delimiter will need to be re-pushed to negate that
-- effect, if desired.
--
-- Out of these four primitive operators have been built various functional
-- abstractions that incorporate one or more operations. On the delimiting
-- side is 'reset', which combines both prompt creation and delimiting. In
-- some papers on the subject (such as /Shift to Control/), each capture
-- operator would be paired with a corresponding delimiter operator (and
-- indeed, a separate CPS transform). However, since prompts are explicitly
-- passed in this implementation, a single delimiter suffices for supporting
-- all capture operators (although 'pushPrompt' will need to be used if one
-- wishes to explicitly push a prompt more than once).
--
-- The simplest control flow operator is 'abort', which, as its name suggests,
-- simply aborts a given sub-continuation. For instance, the second example
-- above can be written:
--
-- > pushPrompt p ((1:) `liftM` (2:) `liftM` abort p (return []))
--
-- The rest of the functions reify the sub-continuation into a function,
-- so that it can be used. The shift/control operators all have similar
-- effects in this regard, but differ as to where they delimit various
-- parts of the resulting computation. Some names may help a bit for the
-- following explanation, so consider:
--
-- > shift p (\f -> e)
--
-- /p/ is, obviously, the prompt; /f/ is the reified continuation, and /e/
-- is the computation that will be run in the aborted context. With these
-- names in mind, the control operators work as follows:
--
-- * 'shift' delimits both /e/ and every invocation of /f/. So, effectively,
--   when using 'shift', control effects can never escape a delimiter, and
--   computations of the form:
--
--    > reset (\p -> <computations with shift p>)
--
--   /look/ pure from the outside.
--
-- * 'control' delimits /e/, but not the sub-continuation in /f/. Thus, if
--   the sub-continuation contains other 'control' invocations, the effects
--   may escape an enclosing delimiter. So, for example:
--
--    > reset (\p -> shift p (\f -> (1:) `liftM` f (return []))
--               >>= \y -> shift p (\_ -> return y))
--
--   yields a value of [1], while replacing the 'shift's with 'control'
--   yields a value of [].
--
-- * 'shift0' delimits /f/, but not /e/. So:
--
--    > reset (\p -> (1:) `liftM` pushPrompt p
--    >                             (shift0 p (\_ -> shift0 p (\_ -> return []))))
--
--    yields [], whereas using 'shift' would yield [1].
--
-- * 'control0' delimits neither /e/ nor /f/, and is, in effect, the reified
--   analogue to using withSubCont and pushSubCont directly.
--
-- For a more complete and in-depth discussion of these four control operators,
-- see /Shift to Control/, by Chung-chieh Shan.
--
-- A small example program follows. It uses delimited continuations to reify a
-- monadic loop into an iterator object. Saving references to old iterators
-- allows one to effecively store references to various points in the traversal.
-- Effectively, this is a simple, restricted case of a generalized zipper.
--
-- > data Iterator r a = I a (CC r (Iterator r a)) | Done
-- >
-- > current :: Iterator r a -> Maybe a
-- > current (I a _) = Just a
-- > current Done    = Nothing
-- > 
-- > next :: Iterator r a -> CC r (Iterator r a)
-- > next (I _ m) = m
-- > next Done    = return Done
-- > 
-- > iterator :: ((a -> CC r ()) -> CC r ()) -> CC r (Iterator r a)
-- > iterator loop = reset $ \p ->
-- >                  loop (\a ->
-- >                     shift p $ \k ->
-- >                         return $ I a (k $ return ())) >> return Done
-- > 
-- > test = do i <- iterator $ forM_ [1..5]
-- >           go [] i
-- >  where
-- >  go l Done = return l
-- >  go l i    = do let (Just a) = current i
-- >                     l' = replicate a a ++ l
-- >                 i' <- next i
-- >                 go l' i'
--
-- The results are what one might expect from such an iterator object:
--
-- > *Test> runCC test
-- > [5,5,5,5,5,4,4,4,4,3,3,3,2,2,1]
