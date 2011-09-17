{-# LANGUAGE GADTs #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.CC.Dynvar
-- Copyright   : (c) Amr Sabry, Chung-chieh Shan and Oleg Kiselyov
-- License     : MIT
--
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (generalized algebraic datatypes)
--
-- An implementation of dynamically scoped variables using multi-prompt
-- delimited control operators. This implementation follows that of the
-- paper /Delimited Dynamic Binding/, by Oleg Kiselyov, Chung-chieh Shan and
-- Amr Sabry (<http://okmij.org/ftp/papers/DDBinding.pdf>), adapting the
-- Haskell implementation (available at
-- <http://okmij.org/ftp/packages/DBplusDC.tar.gz>) to any delimited control
-- monad (in practice, this is likely just CC and CCT m).
--
-- See below for usage examples.
module Control.Monad.CC.Dynvar (
        -- * The Dynvar type
        Dynvar(),
        dnew,
        dref,
        dset,
        dmod,
        dupp,
        dlet,
        mdref,
        module Control.Monad.CC
        -- * examples
        -- $examples
    ) where

import Control.Monad

import Control.Monad.CC

-- | The type of dynamically scoped variables in a given monad
data Dynvar m a where
    Dynvar :: MonadDelimitedCont p s m => p (a -> m a) -> Dynvar m a

-- | Creates a new dynamically scoped variable
dnew :: MonadDelimitedCont p s m => m (Dynvar m a)
dnew = Dynvar `liftM` newPrompt

-- | Reads the value of a dynamically scoped variable
dref :: Dynvar m a -> m a
dref (Dynvar p) = shift p (\f -> return $ \v -> f (return v) >>= ($ v))

-- | Reads the value of a dynamically scoped variable
mdref :: Dynvar m a -> m (Maybe a)
mdref d@(Dynvar p) = do
  t <- isValidPrompt p
  if t
    then Just `liftM` dref d
    else return Nothing

-- | Assigns a value to a dynamically scoped variable
dset :: Dynvar m a -> a -> m a
dset (Dynvar p) newv = shift p (\f -> return $ \v -> f (return v) >>= ($ newv))

-- | Modifies the value of a dynamically scoped variable
dmod :: Dynvar m a -> (a -> a) -> m a
dmod p@(Dynvar _) f = dref p >>= dset p . f

-- | Calls the function, g, with the value of the given Dynvar
dupp :: Dynvar m a -> (a -> m b) -> m b
dupp p@(Dynvar _) g = dref p >>= g

-- | Introduces a new value to the dynamic variable over a block
dlet :: Dynvar m a -> a -> m b -> m b
dlet (Dynvar p) v body = reset (\q ->
                            pushPrompt p (body >>= (\z -> abort q (return z)))
                                >>= ($ v) >>= undefined)

-------------------------------------------------------------------------------
-- $examples
-- The referenced paper provides a full treatment of the behavior of
-- dynamically scoped variables and their interaction with delimited control.
-- However, some examples might provide some intuition. First, a dynamic
-- scoping example:
--
-- > dscope = do p <- dnew
-- >             x <- dlet p 1 $ f p
-- >             y <- dlet p 2 $ f p
-- >             z <- dlet p 3 $ do z1 <- (dlet p 4 $ f p)
-- >                                z2 <- f p
-- >                                return $ z1 + z2
-- >             return $ x + y + z
-- >  where
-- >  f p = dref p
--
-- > *Test> runCC dscope
-- > 10
--
-- In this example, x = 1, y = 2, z1 = 4 and z2 = 3, even though
-- all come are from reading the same dynamically scoped variable. dlet
-- introduces a scope in which references of the given variable take on a
-- given value. As can be seen, shadowing works properly when writing code
-- in this fashion. In many ways, this is like using the reader monad, with
-- 'dref p' == 'ask', and 'dlet p v' == 'local (const v)'. The immediate
-- difference, of course, is that you can have multiple dynamic variables
-- instead of the single threaded environment of the reader monad.
--
-- Of course, one can also use Dynvars mutably, as in the state monad:
--
-- > settest = do p <- dnew
-- >              x <- dlet p 1 $ do x1 <- f p
-- >                                 dset p 2
-- >                                 x2 <- f p
-- >                                 return $ [x1, x2]
-- >              y <- dlet p 0 $ do y1 <- f p
-- >                                 y2 <- dlet p 1 $ do dset p 3
-- >                                                     f p
-- >                                 y3 <- f p
-- >                                 return [y1, y2, y3]
-- >              return $ x ++ y
-- >  where
-- >  f p = dupp p return
-- >
-- > *Test> runCC settest
-- > [1,2,0,3,0]
--
-- So, with analogy to the state monad, 'dref p' == get, and
-- 'dset p v' == 'put v'. Also, as one might expect, such mutations have
-- effects only within the enclosing 'dlet' (and, in fact, an error will
-- result from trying to 'dset' in a scope in which the dynamic var is not
-- bound with 'dlet'). This example also demonstrates the use of the 'dupp'
-- function, to implement the same 'f' function as the first example.
-- Essentially 'dupp p f' = 'dref p >>= f'.
--
-- Now, a bit on the interaction between delimited control and dynamic
-- variables. Consider:
--
-- > test = do p <- dnew
-- >           dlet p 5 (reset (\q -> dlet p 6 (shift q (\f -> dref p))))
-- >
-- > *Test> runCC test
-- > 5
--
-- In this example, '... reset (\q ...' introduces a new delimited context,
-- and '... shift q (\f ...' captures that context abortively. This results
-- in the value of 'dref p' being 5, as the 'dlet p 6' resides in the aborted
-- context. Now, consider a slightly more complex example:
--
-- > test1 = do p <- dnew
-- >            dlet p 5 (reset (\q ->
-- >                         dlet p 6 (shift q (\f ->
-- >                             liftM2 (+) (dref p) (f (dref p))))))
-- >
-- > *Test> runCC test1
-- > 11
--
-- Here we use 'dref p' twice. Once as before, after we have abortively captured
-- the context, and thus, the outer binding of p is showing. However, the term
-- 'f (dref p)' reinstitutes the captured context for its arguments, and thus,
-- there, 'dref p' takes on a value of 6.
--
-- Thus, to sum up, capturing a delimited context captures the dynamic variable
-- bindings *within* that context, but leaves the dynamic bindings *outside*
-- untouched. Similarly, if a context is put back pushed somewhere (for instance,
-- by invoking the function returned by 'shift', it will put the captured
-- dynamic bindings back in place, but will not restore those dynamic bindings
-- outside of the delimited context (it will, instead, use those visible where
-- the context is invoked.

