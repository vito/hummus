{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Main where

import Control.Monad.Trans
import Data.Attoparsec
import Prelude hiding (catch)
import System.Console.Haskeline
import System.Environment (getArgs, getEnv)
import System.FilePath ((</>))
import qualified Data.ByteString as BS

import Hummus.Types
import Hummus.Parser
import Hummus.Runtime
import qualified Hummus.Prelude as Prelude


main :: IO ()
main = do
  as <- getArgs

  runVM $ do
    Prelude.fromGround $ \e -> do
      case as of
        [] -> do
          home <- liftIO (getEnv "HOME")
          runInputT
            (defaultSettings { historyFile = Just (home </> ".hummus_history") })
            (repl "" e)

        [f] -> do
          s <- liftIO (BS.readFile f)
          case parseOnly sexps s of
            Right ss -> mapM_ (evaluate e) ss
            Left m -> error m

        _ -> error "unknown argument form"

      return Inert

    return ()

-- TODO: super hacky
instance MonadException (VM ans) where
  catch x _ = x -- TODO
  block x = x -- TODO
  unblock x = x -- TODO

repl :: String -> Value ans -> InputT (VM ans) ()
repl p e = do
  mi <- getInputLine (if null p then "Hummus> " else "....... ")

  case mi of
    Just i ->
      case parse sexps (BS.pack . map (toEnum . fromEnum) $ p ++ i) of
        Done _ ss -> finish ss

        Fail rest context message -> do
          outputStrLn "Parse error!"
          outputStrLn ("at: " ++ show rest)

          if not (null context)
            then outputStrLn "Context:"
            else return ()

          mapM_ (outputStrLn . ("  " ++)) context
          outputStrLn message
          repl "" e

        Partial f ->
          case f "" of
            Done _ ss -> finish ss
            _ -> repl (p ++ i ++ "\n") e

    Nothing -> return ()
  where
    finish ss = do
      v <- lift (evaluateSequence e ss)
      String s <- lift (evaluate e (Pair (Symbol "send") (Pair v (Pair (Symbol "->string") Null))))
      outputStrLn s
      repl "" e
