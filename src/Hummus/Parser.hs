{-# LANGUAGE OverloadedStrings #-}
module Hummus.Parser where

import Control.Applicative hiding (many)
import Data.Attoparsec as A
import Data.Attoparsec.Char8 (isSpace_w8, isDigit_w8)
import qualified Data.ByteString as BS

import Hummus.Types


whitespace :: Parser ()
whitespace = skipMany $ choice
  [ takeWhile1 isSpace_w8 >> return ()
  , skipMany1 comment
  ]

comment :: Parser ()
comment = do
  string ";"
  manyTill anyWord8 (endOfLine <|> endOfInput)
  return ()
  where
    endOfLine = string "\n" >> return ()

sexps :: Parser [Value ans]
sexps = manyTill (whitespace *> sexp <* whitespace) endOfInput <?> "sexps"

sexp :: Parser (Value ans)
sexp = choice [hNumber, hString, hConstant, hSymbol, hList] <?> "sexp"

toString :: BS.ByteString -> String
toString = map (toEnum . fromEnum) . BS.unpack

hNumber :: Parser (Value ans)
hNumber = (do
  d <- satisfy isDigit_w8
  n <- loop (fromIntegral (fromEnum d - fromEnum '0'))
  return (Number (fromIntegral n))) <?> "number"
  where
    loop :: Integer -> Parser Integer
    loop n = choice
      [ do
          d <- satisfy isDigit_w8
          loop (n * 10 + (fromIntegral $ fromEnum d - fromEnum '0'))
      , return n
      ]

hString :: Parser (Value ans)
hString = (string "\"" *> fmap (String . toString) inString) <?> "string"
  where
    inString = do
      cs <- A.takeWhile (not . inClass "\\\"")
      choice
        [ do
            string "\""
            return cs
        , do
            string "\\\""
            rest <- inString
            return (BS.concat [cs, "\"", rest])
        , fail "unknown escape"
        ]

hConstant :: Parser (Value ans)
hConstant = string "#" *>
  choice
    [ string "t" >> return (Boolean True)
    , string "f" >> return (Boolean False)
    , string "ignore" >> return Ignore
    , string "inert" >> return Inert
    ] <?> "constant"

hSymbol :: Parser (Value ans)
hSymbol = (fmap (Symbol . toString) $ takeWhile1 validChar) <?> "symbol"
  where
    validChar = inClass  "a-zA-Z0-9-<>/?\\|~!@#$%^&*=+_-"

hList :: Parser (Value ans)
hList = (string "(" *> pairs <* string ")") <?> "list"
  where
    pairs = whitespace *> choice
      [ do
          a <- sexp
          whitespace
          string "."
          whitespace
          b <- sexp
          return (Pair a b)
      , do
          a <- sexp
          ps <- pairs
          return (Pair a ps)
      , do
          x <- sexp
          return (Pair x Null)
      , return Null
      ] <* whitespace
