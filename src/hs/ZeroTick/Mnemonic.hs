{-# LANGUAGE TypeSynonymInstances #-}
module ZeroTick.Mnemonic
  ( Right (..)
  , Mnemonic (..)
  , parseMnemonic
  )
  where

import Control.Applicative

import Data.Time
import Data.Time.Parsers

import Data.Attoparsec.Char8 as A
import Data.ByteString.Char8 as B

yyyymmddParser :: Parser Day
yyyymmddParser = withDefaultOptions yyyymmdd

data Right = Call | Put deriving Show

data Mnemonic = Stock  { mSymbol   :: String
                       , mExchange :: String
                       , mCurrency :: String
                       }
              | Option { mSymbol   :: String
                       , mExpiry   :: Day
                       , mRight    :: Right
                       , mStrike   :: Double
                       , mExchange :: String
                       , mCurrency :: String
                       }
              | Future { mSymbol   :: String
                       , mExpiry   :: Day
                       , mExchange :: String
                       , mCurrency :: String
                       }
              deriving Show

fieldParser :: Parser a -> Parser a
fieldParser = (<* skipSpace)

stringField :: Parser String
stringField = fieldParser $ B.unpack <$> A.takeWhile (not . isSpace)

doubleField :: Parser Double
doubleField = fieldParser rational

expiryField :: Parser Day
expiryField = fieldParser yyyymmddParser

rightField :: Parser Right
rightField = stringField >>= \right -> case right of
  "P" -> return Put
  "C" -> return Call
  _   -> fail $ "Unrecognized option right type " ++ right

mnemonicParser :: Parser Mnemonic
mnemonicParser = stringField >>= \cls -> case cls of
  "STK" -> Stock  <$> stringField
                  <*> stringField
                  <*> stringField
  "OPT" -> Option <$> stringField
                  <*> expiryField
                  <*> rightField
                  <*> doubleField
                  <*> stringField
                  <*> stringField
  "FUT" -> Future <$> stringField
                  <*> expiryField
                  <*> stringField
                  <*> stringField
  _     -> fail "Error"

class ToByteString a where
  toByteString :: a -> B.ByteString

instance ToByteString B.ByteString where
  toByteString = id

instance ToByteString String where
  toByteString = B.pack

parseMnemonic :: (ToByteString a) => a -> Either String Mnemonic
parseMnemonic s = parseOnly mnemonicParser (toByteString s)
