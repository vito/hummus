{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.CC.Prompt
-- Copyright   : (c) R. Kent Dybvig, Simon L. Peyton Jones and Amr Sabry
-- License     : MIT
--
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (rank-2 types, generalized algebraic datatypes)
--
-- A monadic treatment of delimited continuations.
--
--    Adapted from the paper
--      /A Monadic Framework for Delimited Continuations/,
--    by R. Kent Dybvig, Simon Peyton Jones and Amr Sabry
--      (<http://www.cs.indiana.edu/~sabry/papers/monadicDC.pdf>)
--
-- This module implements the generation of unique prompt names to be used
-- as delimiters.
module Control.Monad.CC.Prompt (
        -- * P, The prompt generation monad
        P,
        -- * The Prompt type
        Prompt,
        runP,
        newPromptName,
        eqPrompt,
        -- * A type equality datatype
        Equal(..)
    ) where

import Control.Monad.State
import Control.Monad.Reader

import Unsafe.Coerce

-- | The prompt type, parameterized by two types:
-- * ans : The region identifier, used to ensure that prompts are only used
-- within the same context in which they are created.
--
-- * a : The type of values that may be returned 'through' a given prompt.
-- For instance, only prompts of type 'Prompt r a' may be pushed onto a
-- computation of type 'CC r a'.
newtype Prompt ans a = Prompt Int

-- | The prompt generation monad. Represents the type of computations that
-- make use of a supply of unique prompts.
newtype P ans m a = P { unP :: StateT Int m a }
    deriving (Functor, Monad, MonadTrans, MonadState Int, MonadReader r)

-- | Runs a computation that makes use of prompts, yielding a result in the
-- underlying monad.
runP :: (Monad m) => P ans m ans -> m ans
runP p = evalStateT (unP p) 0

-- | Generates a new, unique prompt
newPromptName :: (Monad m) => P ans m (Prompt ans a)
newPromptName = do i <- get ; put (succ i) ; return (Prompt i)

-- | A datatype representing type equality. The EQU constructor can
-- be used to provide evidence that two types are equivalent.
data Equal a b where
    EQU :: Equal a a
    NEQ :: Equal a b

-- Unfortunately, the type system cannot check that the value of two prompts being
-- equal ensures the equality of their types, so unsafeCoerce must be used.

-- | Tests to determine if two prompts are equal. If so, it provides
-- evidence of that fact, in the form of an /Equal/.
eqPrompt :: Prompt ans a -> Prompt ans b -> Equal a b
eqPrompt (Prompt p1) (Prompt p2)
    | p1 == p2  = unsafeCoerce EQU
    | otherwise = NEQ

