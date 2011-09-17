{-# LANGUAGE GADTs #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.CC.Seq
-- Copyright   : (c) R. Kent Dybvig, Simon L. Peyton Jones and Amr Sabry
-- License     : MIT
--
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (generalized algebraic datatypes)
--
-- A monadic treatment of delimited continuations.
--
--    Adapted from the paper
--      /A Monadic Framework for Delimited Continuations/,
--    by R. Kent Dybvig, Simon Peyton Jones and Amr Sabry
--      (<http://www.cs.indiana.edu/~sabry/papers/monadicDC.pdf>)
--
-- This module implements the generalized sequence type used as a stack of
-- frames representation of the delimited continuations.
module Control.Monad.CC.Seq (
        -- * Sequence datatype
        Seq(..),
        -- * Sub-sequences
        SubSeq,
        appendSubSeq,
        pushSeq,
        splitSeq,
        inSeq,
    ) where

import Control.Monad.CC.Prompt

-- | This is a generalized sequence datatype, parameterized by three types:
-- seg : A constructor for segments of the sequence. 
--
-- ans : the type resulting from applying all the segments of the sequence.
-- Also used as a region parameter.
--
-- a   : The type expected as input to the sequence of segments.
data Seq seg ans a where
    EmptyS  :: Seq seg ans ans
    PushP   :: Prompt ans a -> Seq seg ans a -> Seq seg ans a
    PushSeg :: seg ans a b -> Seq seg ans b -> Seq seg ans a

-- | A type representing a sub-sequence, which may be appended to a sequence
-- of appropriate type. It represents a sequence that takes values of type
-- a to values of type b, and may be pushed onto a sequence that takes values
-- of type b to values of type ans.
type SubSeq seg ans a b = Seq seg ans b -> Seq seg ans a

-- | The null sub-sequence
emptySubSeq :: SubSeq seg ans a a
emptySubSeq = id

-- | Concatenate two subsequences
appendSubSeq :: SubSeq seg ans a b -> SubSeq seg ans b c -> SubSeq seg ans a c
appendSubSeq = (.)

-- | Push a sub-sequence onto the front of a sequence
pushSeq :: SubSeq seg ans a b -> Seq seg ans b -> Seq seg ans a
pushSeq = ($)

-- | Splits a sequence at the given prompt into a sub-sequence, and
-- the rest of the sequence
splitSeq :: Prompt ans b -> Seq seg ans a -> (SubSeq seg ans a b, Seq seg ans b)
splitSeq _ EmptyS = error "Prompt was not found on the stack."
splitSeq p (PushP p' sk) =
    case eqPrompt p' p of
         EQU -> (emptySubSeq, sk)
         NEQ -> case splitSeq p sk of
                     (subk, sk') -> (appendSubSeq (PushP p') subk, sk')
splitSeq p (PushSeg seg sk) =
    case splitSeq p sk of
         (subk, sk') -> (appendSubSeq (PushSeg seg) subk, sk')


inSeq :: Prompt ans b -> Seq seg ans a -> Bool
inSeq _ EmptyS = False
inSeq p (PushP p' sk) =
    case eqPrompt p' p of
         EQU -> True
         NEQ -> inSeq p sk
inSeq p (PushSeg _ sk) = inSeq p sk
