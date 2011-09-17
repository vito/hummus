module Hummus.Runtime where

import Control.Monad
import Control.Monad.CC.Dynvar
import Control.Monad.Trans
import Data.Maybe (catMaybes, isJust)
import qualified Data.HashTable.IO as H

import Hummus.Types


evaluate :: Value ans -> Value ans -> VM ans (Value ans)
evaluate env (Pair a b) = do
  x <- evaluate env a
  if isCombiner x
    then apply env x b
    else error ("not a combiner: " ++ show x)
evaluate env (Symbol s) = do
  mv <- fetch env s
  case mv of
    Just v -> return v
    Nothing -> error ("undefined: " ++ s)
evaluate env o@(Operative { oStaticEnvironment = Nothing }) =
  return o { oStaticEnvironment = Just env }
evaluate _ x = return x

evaluateSequence :: Value ans -> [Value ans] -> VM ans (Value ans)
evaluateSequence _ [] = return Null
evaluateSequence e [s] = evaluate e s
evaluateSequence e (s:ss) = evaluate e s >> evaluateSequence e ss

evaluateAll :: Value ans -> Value ans -> VM ans (Value ans)
evaluateAll env (Pair a b) = do
  ea <- evaluate env a
  eb <- evaluateAll env b
  return (Pair ea eb)
evaluateAll _ x = return x

apply :: Value ans -> Value ans -> Value ans -> VM ans (Value ans)
apply env (CoreOperative f) as = f as env
apply env (Operative fs ef b se) as = do
  newEnv <- newEnvironment (catMaybes [se])

  define newEnv fs as
  define newEnv ef env

  evaluate newEnv b
apply env (Applicative x) vs = do
  as <- evaluateAll env vs
  apply env x as
apply _ (Dynvar d x) _ = liftM (maybe x id) (mdref d)
apply _ v _ = error ("cannot apply: " ++ show v)

define :: Value ans -> Value ans -> Value ans -> VM ans ()
define env@(Environment ht _) p v =
  case p of
    Ignore -> return ()

    Symbol n -> liftIO (H.insert ht n v)

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

binds :: Value ans -> String -> VM ans Bool
binds e n = liftM isJust (fetch e n)

fetch :: Value ans -> String -> VM ans (Maybe (Value ans))
fetch (Environment ht ps) n = do
  l <- liftIO (H.lookup ht n)
  case l of
    Just v -> return (Just v)
    Nothing -> do
      up <- mapM (flip fetch n) ps
      case catMaybes up of
        (x:_) -> return (Just x)
        [] -> return Nothing
fetch v n = error ("cannot fetch " ++ show n ++ " from " ++ show v)
