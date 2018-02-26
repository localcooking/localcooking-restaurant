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
  -- = CandleStickInput CandleStickInput

instance FromJSON SubsInput where
  parseJSON x = fail "D:" -- CandleStickInput <$> parseJSON x

data SubsOutput
  -- = CandleStickOutput CandleStickOutput

instance ToJSON SubsOutput where
  toJSON x = String "D:" -- case x of
    -- CandleStickOutput x' -> toJSON x'
