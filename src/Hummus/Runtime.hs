module Hummus.Runtime where

import Data.Maybe (catMaybes, isJust)
import qualified Data.HashTable.IO as H

import Hummus.Types


evaluate :: Value -> Value -> IO Value
evaluate env (Pair a b) = do
  x <- evaluate env a
  if isOperative x
    then apply env x b
    else
      case x of
        Applicative c -> do
          as <- evaluateAll env b
          apply env c as

        _ -> error ("not a combiner: " ++ show x)
evaluate env (Symbol s) = do
  mv <- fetch env s
  case mv of
    Just v -> return v
    Nothing -> error ("undefined: " ++ s)
evaluate env o@(Operative { oStaticEnvironment = Nothing }) =
  return o { oStaticEnvironment = Just env }
evaluate _ x = return x

evaluateSequence :: Value -> [Value] -> IO Value
evaluateSequence _ [] = return Null
evaluateSequence e [s] = evaluate e s
evaluateSequence e (s:ss) = evaluate e s >> evaluateSequence e ss

evaluateAll :: Value -> Value -> IO Value
evaluateAll env (Pair a b) = do
  ea <- evaluate env a
  eb <- evaluateAll env b
  return (Pair ea eb)
evaluateAll _ x = return x

apply :: Value -> Value -> Value -> IO Value
apply env (CoreOperative f) as = f as env
apply env (Operative fs ef b se) as = do
  newEnv <- newEnvironment (catMaybes [se])

  define newEnv fs as
  define newEnv ef env

  evaluate newEnv b
apply _ v _ = error ("cannot apply: " ++ show v)

define :: Value -> Value -> Value -> IO ()
define env@(Environment ht _) p v =
  case p of
    Ignore -> return ()

    Symbol n -> H.insert ht n v

    Null ->
      case v of
        Null -> return ()
        _ -> error ("mismatch: " ++ show (p, v))

    Pair pa pb ->
      case v of
        Pair va vb -> do
          define env pa va
          define env pb vb

        _ -> error ("mismatch: " ++ show (p, v))

    _ -> error ("unknown pattern: " ++ show p)
define _ _ _ = error "invalid definition target"

binds :: Value -> String -> IO Bool
binds e n = fmap isJust (fetch e n)

fetch :: Value -> String -> IO (Maybe Value)
fetch (Environment ht ps) n = do
  l <- H.lookup ht n
  case l of
    Just v -> return (Just v)
    Nothing -> do
      up <- mapM (flip fetch n) ps
      case catMaybes up of
        (x:_) -> return (Just x)
        [] -> return Nothing
fetch v n = error ("cannot fetch " ++ show n ++ " from " ++ show v)
