{-# LANGUAGE
    OverloadedStrings
  #-}

module LocalCooking.WebSocket where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object, Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))


data LocalCookingOp
--   = LocalCookingOpChartData

-- koredexOp :: Atto.Parser LocalCookingOp
-- koredexOp = LocalCookingOpChartData <$ Atto.string "chartData"


data LocalCookingInput
  = LocalCookingPing
  deriving (Show)

instance FromJSON LocalCookingInput where
  parseJSON (String x)
    | x == "ping" = pure LocalCookingPing
    | otherwise = fail "Not a LocalCookingPing"
  parseJSON x = typeMismatch "LocalCookingInput" x


data LocalCookingOpResult
  -- = LocalCookingOpResultChartData StatsDB

-- instance ToJSON LocalCookingOpResult where
--   toJSON x = case x of
--     LocalCookingOpResultChartData xs -> object
--       [ "chartData" .= xs
--       ]


-- data LocalCookingLoginResult
--   = LocalCookingLoginSuccess User
--   | LocalCookingLoginFailure

-- instance ToJSON LocalCookingLoginResult where
--   toJSON x = case x of
--     LocalCookingLoginSuccess u -> object ["success" .= u]
--     LocalCookingLoginFailure -> String "failure"


data LocalCookingOutput
  = LocalCookingPong

instance ToJSON LocalCookingOutput where
  toJSON x = case x of
    -- LocalCookingLoginResult r -> object
    --   [ "loginResult" .= r
    --   ]
    -- LocalCookingOpResult x -> object
    --   [ "op" .= x
    --   ]
    -- LocalCookingSubsOutput y -> toJSON y
    LocalCookingPong -> toJSON (String "pong")
