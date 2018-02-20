{-# LANGUAGE
    OverloadedStrings
  #-}

module LocalCooking.Subs where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, Value (..), (.=))
import Data.Aeson.Types (typeMismatch)
import Data.Vector (Vector)
import Control.Monad (unless)
import Control.Applicative ((<|>))


data SubsInput
  = CandleStickInput CandleStickInput
  deriving (Show)

instance FromJSON SubsInput where
  parseJSON x = CandleStickInput <$> parseJSON x

data SubsOutput
  = CandleStickOutput CandleStickOutput

instance ToJSON SubsOutput where
  toJSON x = case x of
    CandleStickOutput x' -> toJSON x'


data CandleStickInput
  = CandleStickSubscribe
  | CandleStickUnsubscribe
  deriving (Show)

instance FromJSON CandleStickInput where
  parseJSON (Object o) = do
    let sub = do
          x <- o .: "subscribe"
          unless (x == String "candlestick") $
            fail "Not a candlestick"
          pure CandleStickSubscribe
        unsub = do
          x <- o .: "unsubscribe"
          unless (x == String "candlestick") $
            fail "Not a candlestick"
          pure CandleStickUnsubscribe
    sub <|> unsub
  parseJSON x = typeMismatch "CandleStickInput" x



data CandleStickOutput

instance ToJSON CandleStickOutput where
  toJSON x = toJSON (String "D:")
