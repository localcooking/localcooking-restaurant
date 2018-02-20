{-# LANGUAGE
    OverloadedStrings
  #-}

module LocalCooking.WebSocket where

import LocalCooking.Subs (SubsInput, SubsOutput)
import LocalCooking.Auth (UserID, SignedChallenge)
import Database (User)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object, Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))


data LocalCookingOp
--   = LocalCookingOpChartData

-- koredexOp :: Atto.Parser LocalCookingOp
-- koredexOp = LocalCookingOpChartData <$ Atto.string "chartData"


data LocalCookingInput
  = LocalCookingLogin
      { koredexLoginUserID :: UserID
      , koredexLoginSignedChallenge :: SignedChallenge
      }
  -- | LocalCookingOp LocalCookingOp
  | LocalCookingSubsInput SubsInput
  | LocalCookingPing
  deriving (Show)

instance FromJSON LocalCookingInput where
  parseJSON x@(Object o) = do
    let parseLogin = do
          o' <- o .: "login"
          case o' of
            Object o'' -> LocalCookingLogin <$> (o'' .: "userID") <*> (o'' .: "signedChallenge")
            _ -> typeMismatch "LocalCookingInput" x
        -- parseOp = LocalCookingOp <$> (attoAeson koredexOp =<< o .: "op")
        parseSubs = LocalCookingSubsInput <$> parseJSON x
    parseLogin <|> parseSubs
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


data LocalCookingLoginResult
  = LocalCookingLoginSuccess User
  | LocalCookingLoginFailure

instance ToJSON LocalCookingLoginResult where
  toJSON x = case x of
    LocalCookingLoginSuccess u -> object ["success" .= u]
    LocalCookingLoginFailure -> String "failure"


data LocalCookingOutput
  = LocalCookingLoginResult LocalCookingLoginResult
  -- | LocalCookingOpResult LocalCookingOpResult
  | LocalCookingSubsOutput SubsOutput
  | LocalCookingPong

instance ToJSON LocalCookingOutput where
  toJSON x = case x of
    LocalCookingLoginResult r -> object
      [ "loginResult" .= r
      ]
    -- LocalCookingOpResult x -> object
    --   [ "op" .= x
    --   ]
    LocalCookingSubsOutput y -> toJSON y
    LocalCookingPong -> toJSON (String "pong")
