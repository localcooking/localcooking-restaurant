{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  #-}

module Server.Dependencies.DeviceToken where

import Types (AppM)
import Types.Env (Env (..), Devices (..))
import LocalCooking.Device (DeviceToken, newDeviceToken, verifyDeviceToken)

import Web.Dependencies.Sparrow (Server, ServerContinue (..), ServerReturn (..), ServerArgs (..))
import Data.Text (Text)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), (.:), Value (..))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)


data DeviceTokenInitIn
  = DeviceTokenInitInNew
    { deviceTokenInitInNewName :: Text
    }
  | DeviceTokenInitInVerify DeviceToken

instance FromJSON DeviceTokenInitIn where
  parseJSON (Object o) = do
    let new = DeviceTokenInitInNew <$> o .: "new"
        verify = DeviceTokenInitInVerify <$> o .: "verify"
    new <|> verify
  parseJSON x = typeMismatch "DeviceTokenInitIn" x


data DeviceTokenInitOut
  = DeviceTokenInitOut DeviceToken

instance ToJSON DeviceTokenInitOut where
  toJSON (DeviceTokenInitOut x) = toJSON x


data DeviceTokenDeltaIn

instance FromJSON DeviceTokenDeltaIn where
  parseJSON = typeMismatch "DeviceTokenDeltaIn"


data DeviceTokenDeltaOut
  = DeviceTokenDeltaOutRevoked -- TODO causes frontend system-wide logout

instance ToJSON DeviceTokenDeltaOut where
  toJSON DeviceTokenDeltaOutRevoked = String "device-revoked"


deviceTokenServer :: Server AppM DeviceTokenInitIn
                                 DeviceTokenInitOut
                                 DeviceTokenDeltaIn
                                 DeviceTokenDeltaOut
deviceTokenServer initIn = do
  Env{envDevices = Devices{devicesNames}} <- ask

  mToken <- case initIn of
    DeviceTokenInitInNew name ->
      Just <$> liftIO (newDeviceToken devicesNames name)
    DeviceTokenInitInVerify token -> do
      exists <- liftIO (verifyDeviceToken devicesNames token)
      pure $ if exists
             then Just token
             else Nothing

  case mToken of
    Nothing -> pure Nothing
    Just token ->
      pure $ Just ServerContinue
        { serverOnUnsubscribe = pure () -- FIXME should never unsubscribe?
        , serverContinue = \_ -> pure ServerReturn
          { serverInitOut = DeviceTokenInitOut token
          , serverOnOpen = \ServerArgs{} -> pure Nothing
          , serverOnReceive = \_ _ -> pure () -- not possible
          }
        }
