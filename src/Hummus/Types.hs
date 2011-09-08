module Hummus.Types where

import qualified Data.HashTable.IO as H


data Value
  = Applicative Value
  | Boolean Bool
  | Environment (H.LinearHashTable String Value) [Value]
  | Ignore
  | Inert
  | Null
  | Number Integer
  | Operative
      { oFormals :: Value
      , oEnvironmentFormal :: Value
      , oBody :: Value
      , oStaticEnvironment :: Maybe Value
      }
  | CoreOperative (Value -> Value -> IO Value)
  | Pair Value Value
  | String String
  | Symbol String


instance Show Value where
  show (Applicative v) = "<applicative " ++ show v ++ ">"
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (Environment _ _) = "<environment>"
  show Ignore = "#ignore"
  show Inert = "#inert"
  show Null = "()"
  show (Number n) = show n
  show (Operative { oFormals = fs, oEnvironmentFormal = ef, oBody = b }) =
    "<operative " ++ show fs ++ " " ++ show ef ++ " " ++ show b ++ ">"
  show (CoreOperative _) = "<core operative>"
  show (Pair a b) = "(" ++ showPair (Pair a b) ++ ")"
    where
      showPair (Pair a b)
        | isPair b = show a ++ " " ++ showPair b
        | isNull b = show a
        | otherwise = show a ++ " . " ++ show b
      showPair x = show x
  show (String s) = show s
  show (Symbol s) = s


instance Eq Value where
  Applicative a == Applicative b = a == b
  Boolean a == Boolean b = a == b
  Environment aht aps == Environment bht bps = False
  Ignore == Ignore = True
  Inert == Inert = True
  Null == Null = True
  Number a == Number b = a == b
  Operative afs aef ab ase == Operative bfs bef bb bse =
    afs == bfs && aef == bef && ab == bb && ase == bse
  CoreOperative _ == CoreOperative _ = False
  Pair ah at == Pair bh bt = ah == bh && at == bt
  String a == String b = a == b
  Symbol a == Symbol b = a == b


newEnvironment :: [Value] -> IO Value
newEnvironment ps = do
  ht <- H.new
  return (Environment ht ps)


toList :: Value -> [Value]
toList (Pair a b) = a : toList b
toList Null = []
toList x = error ("cannot toList: " ++ show x)


isTrue :: Value -> Bool
isTrue (Boolean True) = True
isTrue _ = False

isFalse :: Value -> Bool
isFalse (Boolean False) = True
isFalse _ = False

isBoolean :: Value -> Bool
isBoolean (Boolean _) = True
isBoolean _ = False

isApplicative :: Value -> Bool
isApplicative (Applicative _) = True
isApplicative _ = False

isSymbol :: Value -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isIgnore :: Value -> Bool
isIgnore Ignore = True
isIgnore _ = False

isNull :: Value -> Bool
isNull Null = True
isNull _ = False

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

isPair :: Value -> Bool
isPair (Pair _ _) = True
isPair _ = False

isEnvironment :: Value -> Bool
isEnvironment (Environment _ _) = True
isEnvironment _ = False

isInert :: Value -> Bool
isInert Inert = True
isInert _ = False

isOperative :: Value -> Bool
isOperative (CoreOperative _) = True
isOperative (Operative {}) = True
isOperative _ = False
