{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs,
             UndecidableInstances #-}
module Hummus.Types where

import Control.Monad.CC
import Control.Monad.CC.Dynvar
import Control.Monad.CC.Prompt
import Control.Monad.Trans
import Data.IORef
import qualified Data.HashTable.IO as H


newtype VM ans a = VM { unVM :: CCT ans IO a }

runVM :: (forall ans. VM ans a) -> IO a
runVM v = runCCT (unVM v)

instance Monad (VM ans) where
  return = VM . return
  x >>= y = VM (unVM x >>= unVM . y)

instance MonadIO (VM ans) where
  liftIO x = VM (liftIO x)

instance MonadDelimitedCont (Prompt ans) (SubCont ans IO) (VM ans) where
  newPrompt = VM newPrompt
  pushPrompt p x = VM (pushPrompt p (unVM x))
  withSubCont p f = VM (withSubCont p (unVM . f))
  pushSubCont s x = VM (pushSubCont s (unVM x))

data Value ans
  = Applicative (Value ans)
  | Boolean Bool
  | CoreOperative (Value ans -> Value ans -> VM ans (Value ans))
  | Dynvar (Dynvar (VM ans) (Value ans))
  | Encapsulation
      { eID :: IORef ()
      , eValue :: IORef (Value ans) -- for shallow equality check
      }
  | Environment (H.LinearHashTable String (Value ans)) [Value ans]
  | Ignore
  | Inert
  | Null
  | Number Integer
  | Operative
      { oFormals :: Value ans
      , oEnvironmentFormal :: Value ans
      , oBody :: Value ans
      , oStaticEnvironment :: Maybe (Value ans)
      }
  | Pair (Value ans) (Value ans)
  | Prompt (Prompt ans (Value ans))
  | String String
  | SubContinuation (SubCont ans IO (Value ans) (Value ans))
  | Symbol String


instance forall ans. Show (Value ans) where
  show (Applicative v) = "<applicative " ++ show v ++ ">"
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (CoreOperative _) = "<core operative>"
  show (Dynvar _) = "<dynamic variable>"
  show (Encapsulation {}) = "<encapsulation>"
  show (Environment _ _) = "<environment>"
  show Ignore = "#ignore"
  show Inert = "#inert"
  show Null = "()"
  show (Number n) = show n
  show (Operative { oFormals = fs, oEnvironmentFormal = ef, oBody = b }) =
    "<operative " ++ show fs ++ " " ++ show ef ++ " " ++ show b ++ ">"
  show p@(Pair _ _) = "(" ++ showPair p ++ ")"
    where
      showPair (Pair a b)
        | isPair b = show a ++ " " ++ showPair b
        | isNull b = show a
        | otherwise = show a ++ " . " ++ show b
      showPair x = show x
  show (Prompt _) = "<prompt>"
  show (String s) = show s
  show (SubContinuation _) = "<subcontinuation>"
  show (Symbol s) = s


instance forall ans. Eq (Value ans) where
  Applicative a == Applicative b = a == b
  Boolean a == Boolean b = a == b
  Encapsulation aid av == Encapsulation bid bv =
    aid == bid && av == bv
  Ignore == Ignore = True
  Inert == Inert = True
  Null == Null = True
  Number a == Number b = a == b
  Operative afs aef ab ase == Operative bfs bef bb bse =
    afs == bfs && aef == bef && ab == bb && ase == bse
  Pair ah at == Pair bh bt = ah == bh && at == bt
  Prompt a == Prompt b =
    case eqPrompt a b of
      EQU -> True
      NEQ -> False
  String a == String b = a == b
  Symbol a == Symbol b = a == b
  _ == _ = False


newEnvironment :: [Value ans] -> VM ans (Value ans)
newEnvironment ps = do
  ht <- liftIO H.new
  return (Environment ht ps)


toList :: Value ans -> [Value ans]
toList (Pair a b) = a : toList b
toList Null = []
toList x = error ("cannot toList: " ++ show x)


isTrue :: Value ans -> Bool
isTrue (Boolean True) = True
isTrue _ = False

isFalse :: Value ans -> Bool
isFalse (Boolean False) = True
isFalse _ = False

isBoolean :: Value ans -> Bool
isBoolean (Boolean _) = True
isBoolean _ = False

isApplicative :: Value ans -> Bool
isApplicative (Applicative _) = True
isApplicative _ = False

isSymbol :: Value ans -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isIgnore :: Value ans -> Bool
isIgnore Ignore = True
isIgnore _ = False

isNull :: Value ans -> Bool
isNull Null = True
isNull _ = False

isNumber :: Value ans -> Bool
isNumber (Number _) = True
isNumber _ = False

isPair :: Value ans -> Bool
isPair (Pair _ _) = True
isPair _ = False

isEnvironment :: Value ans -> Bool
isEnvironment (Environment _ _) = True
isEnvironment _ = False

isInert :: Value ans -> Bool
isInert Inert = True
isInert _ = False

isOperative :: Value ans -> Bool
isOperative (CoreOperative _) = True
isOperative (Operative {}) = True
isOperative _ = False

isPrompt :: Value ans -> Bool
isPrompt (Prompt _) = True
isPrompt _ = False

isDynvar :: Value ans -> Bool
isDynvar (Dynvar _) = True
isDynvar _ = False

isCombiner :: Value ans -> Bool
isCombiner x = isApplicative x || isOperative x || isDynvar x
