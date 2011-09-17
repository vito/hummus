{-# LANGUAGE RankNTypes #-}
module Hummus.Prelude where

import Control.Monad
import Control.Monad.CC
import Control.Monad.CC.Dynvar
import Control.Monad.Trans
import Data.Attoparsec
import Data.IORef
import Data.Time
import qualified Data.ByteString as BS

import Hummus.Types
import Hummus.Parser
import Hummus.Runtime

import Paths_hummus


new :: VM ans (Value ans)
new = do
  env <- newEnvironment []

  defn env "make-encapsulation-type" $ \Null _ -> do
    i <- liftIO (newIORef ())

    let cons =
          Applicative . CoreOperative $ \(Pair a Null) _ -> do
            vr <- liftIO (newIORef a)
            return Encapsulation { eID = i, eValue = vr }

        test =
          Applicative . CoreOperative $ \(Pair a Null) _ ->
            case a of
              Encapsulation { eID = eid } -> return (Boolean (eid == i))
              _ -> return (Boolean False)

        decons =
          Applicative . CoreOperative $ \(Pair a Null) _ ->
            case a of
              Encapsulation { eID = eid, eValue = vr } | eid == i ->
                liftIO (readIORef vr)

              _ -> error "encapsulation type mismatch"

    return (Pair cons (Pair test (Pair decons Null)))

  defn env "reset" $ \(Pair b _) e ->
    reset $ \p ->
      apply e b (Pair (Prompt p) Null)

  defn env "make-dynvar" $ \(Pair a _) _ ->
    liftM (flip Dynvar a) dnew

  defn env "put!" $ \(Pair (Dynvar d _) (Pair b Null)) _ ->
    dset d b

  def env "with" $ \(Pair as bs) e -> do
    letDyn e (map toList (toList as)) (toList bs)

  defn env "shift" $ \(Pair a (Pair b _)) e -> do
    Prompt p <- evaluate e a
    shift p $ \f ->
      let app = Applicative . CoreOperative $ \(Pair x _) _ -> f (return x)
      in apply e b (Pair app Null)

  defn env "control" $ \(Pair a (Pair b _)) e -> do
    Prompt p <- evaluate e a
    control p $ \f ->
      let app = Applicative . CoreOperative $ \(Pair x _) _ -> f (return x)
      in apply e b (Pair app Null)

  defn env "shift0" $ \(Pair a (Pair b _)) e -> do
    Prompt p <- evaluate e a
    shift0 p $ \f ->
      let app = Applicative . CoreOperative $ \(Pair x _) _ -> f (return x)
      in apply e b (Pair app Null)

  defn env "control0" $ \(Pair a (Pair b _)) e -> do
    Prompt p <- evaluate e a
    control0 p $ \f ->
      let app = Applicative . CoreOperative $ \(Pair x _) _ -> f (return x)
      in apply e b (Pair app Null)

  def env "abort" $ \(Pair a (Pair b _)) e -> do
    Prompt p <- evaluate e a
    abort p (evaluate e b)

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

  defn env "make-environment" $ \parents _ ->
    newEnvironment (toList parents)

  def env "binds?" $ \(Pair a bs) e -> do
    e' <- evaluate e a
    ss <- mapM (\(Symbol s) -> binds e' s) (toList bs)
    return (Boolean (and ss))

  def env "define" $ \(Pair a (Pair b _)) e -> do
    v <- evaluate e b
    define e a v
    return Inert

  defn env "operative?" $ \(Pair a _) _ ->
    return (Boolean (isOperative a))

  defn env "applicative?" $ \(Pair a _) _ ->
    return (Boolean (isApplicative a))

  defn env "dynvar?" $ \(Pair a _) _ ->
    return (Boolean (isDynvar a))

  defn env "combiner?" $ \as _ ->
    return (Boolean (and (map isCombiner (toList as))))

  def env "vau" $ \(Pair a (Pair b (Pair c _))) e ->
    return (Operative a b c (Just e))

  defn env "wrap"  $ \(Pair a _) _ ->
    return (Applicative a)

  defn env "unwrap"  $ \(Pair a _) _ ->
    case a of
      Applicative c -> return c
      _ -> error ("not an applicative: " ++ show a)

  defn env "make-prompt" $ \Null _ -> do
    x <- newPrompt
    return (Prompt x)

  def env "push-prompt" $ \(Pair a bs) e -> do
    Prompt p <- evaluate e a
    pushPrompt p (evaluateSequence e (toList bs))

  defn env "with-sub-cont" $ \(Pair (Prompt p) (Pair x Null)) e -> do
    withSubCont p $ \s ->
      apply e x (Pair (SubContinuation s) Null)

  defn env "push-sub-cont" $ \(Pair a bs) e -> do
    SubContinuation s <- evaluate e a
    pushSubCont s (evaluateSequence e (toList bs))

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
    liftIO (print a)
    return Inert

  def env "time" $ \(Pair a _) e -> do
    before <- liftIO getCurrentTime
    x <- evaluate e a
    after <- liftIO getCurrentTime
    liftIO (print x)
    liftIO (print (diffUTCTime after before))
    return Inert
    
  def env "loop" $ \as e ->
    forever $ evaluateSequence e (toList as)

  defn env "get-hummus-data-file" $ \(Pair (String fn) _) _ -> do
    liftM String (liftIO (getDataFileName fn))

  defn env "load" $ \(Pair (String fn) _) e -> do
    source <- liftIO (BS.readFile fn)
    case parseOnly sexps source of
      Right ss ->
        evaluateSequence e ss

      Left msg ->
        error msg

  bootFile <- liftIO (getDataFileName "kernel/boot.hms")
  boot <- liftIO (BS.readFile bootFile)
  case parseOnly sexps boot of
    Right ss ->
      mapM_ (evaluate env) ss

    Left e ->
      error e

  return env
 where
  def :: Value ans -> String -> (Value ans -> Value ans -> VM ans (Value ans)) -> VM ans ()
  def e n f = define e (Symbol n) (CoreOperative f)

  defn :: Value ans -> String -> (Value ans -> Value ans -> VM ans (Value ans)) -> VM ans ()
  defn e n f = define e (Symbol n) (Applicative $ CoreOperative f)

  letDyn :: Value ans -> [[Value ans]] -> [Value ans] -> VM ans (Value ans)
  letDyn e [] bs = evaluateSequence e bs
  letDyn e ([a, b]:as) bs = do
    Dynvar d _ <- evaluate e a
    v <- evaluate e b
    dlet d v (letDyn e as bs)
  letDyn _ (p:_) _ = error $ "unknown pair: " ++ show p

fromGround :: (Value ans -> VM ans (Value ans)) -> VM ans (Value ans)
fromGround x = do
  e <- new

  reset $ \root -> do
    define e (Symbol "root-prompt") (Prompt root)
    x e
