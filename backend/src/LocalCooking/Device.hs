{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Device where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as BS64
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TimeMap
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Base64 (base64)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async (), async)
import Control.Concurrent.STM (atomically)
import Crypto.Saltine.Core.Box (Nonce, newNonce)
import qualified Crypto.Saltine.Class as NaCl


newtype DeviceToken = DeviceToken
  { getDeviceToken :: Nonce
  } deriving (Eq, Hashable)

instance Show DeviceToken where
  show (DeviceToken x) = T.unpack (T.decodeUtf8 (NaCl.encode x))

deviceTokenParser :: Parser DeviceToken
deviceTokenParser = do
  xs <- base64
  case BS64.decode (T.encodeUtf8 xs) of
    Left e -> fail e
    Right x -> case NaCl.decode x of
      Nothing -> fail "Not a Nonce"
      Just n -> pure (DeviceToken n)

instance ToJSON DeviceToken where
  toJSON (DeviceToken x) = String $ T.decodeUtf8 $ BS64.encode $ NaCl.encode x

instance FromJSON DeviceToken where
  parseJSON = attoAeson deviceTokenParser


-- TODO register device token, get device description, revoke device, etc.

newDeviceToken :: TimeMap DeviceToken Text -> Text -> IO DeviceToken
newDeviceToken tokens name = do
  token <- DeviceToken <$> newNonce
  TimeMap.insert token name tokens
  pure token


verifyDeviceToken :: TimeMap DeviceToken Text -> DeviceToken -> IO Bool
verifyDeviceToken tokens token = do
  mName <- atomically (TimeMap.lookup token tokens)
  case mName of
    Nothing -> pure False
    Just _ -> do
      TimeMap.touch token tokens
      pure True


watchDeviceTokens :: TimeMap DeviceToken Text -> IO (Async ())
watchDeviceTokens tokens = async $ forever $ do
  let minute = 60
      hour = 60 * minute
      day = 24 * hour
      week = 7 * day

      ghcSecond = 10 ^ 6
  TimeMap.filterFromNow week tokens
  threadDelay (ghcSecond * 60)
