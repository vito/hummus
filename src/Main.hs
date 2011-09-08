{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans (liftIO)
import Data.Attoparsec
import Data.Time
import System.Console.Haskeline
import qualified Data.ByteString as BS

import Hummus.Types
import Hummus.Parser
import Hummus.Runtime
import qualified Hummus.Prelude as Prelude


main :: IO ()
main = Prelude.new >>= runInputT defaultSettings . repl ""

repl :: String -> Value -> InputT IO ()
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
      liftIO (evaluateSequence e ss) >>= outputStrLn . show
      repl "" e
