module Hummus.Prelude where

import Data.Attoparsec
import Data.Time
import qualified Data.ByteString as BS

import Hummus.Types
import Hummus.Parser
import Hummus.Runtime

import Paths_hummus


new :: IO Value
new = do
  env <- newEnvironment []

  defn env "boolean?"  $ \(Pair a _) _ ->
    return (Boolean (isBoolean a))

  defn env "eq?"  $ \(Pair a (Pair b _)) _ ->
    return (Boolean (a == b))

  evaluate env (Symbol "eq?") >>= define env (Symbol "equal?")

  defn env "symbol?"  $ \(Pair a _) _ ->
    return (Boolean (isSymbol a))

  defn env "inert?"  $ \(Pair a _) _ ->
    return (Boolean (isInert a))

  defn env "pair?"  $ \(Pair a _) _ ->
    return (Boolean (isPair a))

  defn env "null?"  $ \(Pair a _) _ ->
    return (Boolean (isNull a))

  defn env "cons"  $ \(Pair a (Pair b _)) _ ->
    return (Pair a b)

  def env "if" $ \(Pair a (Pair b (Pair c _))) e -> do
    t <- evaluate e a

    case t of
      Boolean True ->
        evaluate e b

      Boolean False ->
        evaluate e c

      _ -> error ("not a boolean: " ++ show t)

  defn env "environment?"  $ \(Pair a _) _ ->
    return (Boolean (isEnvironment a))

  defn env "ignore?"  $ \(Pair a _) _ ->
    return (Boolean (isIgnore a))

  defn env "number?"  $ \(Pair a _) _ ->
    return (Boolean (isNumber a))

  defn env "eval"  $ \(Pair a (Pair b _)) _ ->
    evaluate b a

  defn env "make-environment"  $ \parents _ ->
    newEnvironment (toList parents)

  def env "binds?" $ \(Pair a bs) e -> do
    e' <- evaluate e a
    ss <- mapM (\(Symbol s) -> binds e' s) (toList bs)
    return (Boolean (and ss))

  def env "define" $ \(Pair a (Pair b _)) e -> do
    v <- evaluate e b
    define e a v
    return Inert

  defn env "operative?"  $ \(Pair a _) _ ->
    return (Boolean (isOperative a))

  defn env "applicative?"  $ \(Pair a _) _ ->
    return (Boolean (isApplicative a))

  def env "vau" $ \(Pair a (Pair b (Pair c _))) e ->
    return (Operative a b c (Just e))

  defn env "wrap"  $ \(Pair a _) _ ->
    return (Applicative a)

  defn env "unwrap"  $ \(Pair a _) _ ->
    case a of
      Applicative c -> return c
      _ -> error ("not an applicative: " ++ show a)

  defn env "=?"  $ \as _ ->
    let allEq (a:b:cs) = a == b && allEq (b:cs)
        allEq _ = True
    in return (Boolean (allEq (toList as)))

  defn env "max"  $ \as _ ->
    let nums = map (\(Number n) -> n) (toList as)
    in return (Number (maximum nums))

  defn env "<?"  $ \(Pair (Number a) (Pair (Number b) _)) _ ->
    return (Boolean (a < b))

  defn env ">?"  $ \(Pair (Number a) (Pair (Number b) _)) _ ->
    return (Boolean (a > b))

  defn env "<=?"  $ \(Pair (Number a) (Pair (Number b) _)) _ ->
    return (Boolean (a <= b))

  defn env ">=?"  $ \(Pair (Number a) (Pair (Number b) _)) _ ->
    return (Boolean (a >= b))

  defn env "+"  $ \as _ ->
    let nums = map (\(Number n) -> n) (toList as)
    in return (Number (sum nums))

  defn env "*"  $ \as _ ->
    let nums = map (\(Number n) -> n) (toList as)
    in return (Number (product nums))

  defn env "-"  $ \(Pair (Number a) (Pair (Number b) _)) _ ->
    return (Number (a - b))

  defn env "print" $ \(Pair a _) _ -> do
    print a
    return Inert

  def env "time" $ \(Pair a _) e -> do
    before <- getCurrentTime
    x <- evaluate e a
    after <- getCurrentTime
    print x
    print (diffUTCTime after before)
    return Inert

  bootFile <- getDataFileName "kernel/boot.hms"
  boot <- BS.readFile bootFile
  case parseOnly sexps boot of
    Right ss ->
      mapM_ (evaluate env) ss

    Left e ->
      error e

  return env
 where
  def e n f = define e (Symbol n) (CoreOperative f)
  defn e n f = define e (Symbol n) (Applicative $ CoreOperative f)
