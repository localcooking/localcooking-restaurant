{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Device where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Hashable (Hashable)
import Crypto.Saltine.Core.Box (Nonce)
import qualified Crypto.Saltine.Class as NaCl


newtype DeviceToken = DeviceToken
  { getDeviceToken :: Nonce
  } deriving (Eq, Hashable)

instance Show DeviceToken where
  show (DeviceToken x) = T.unpack (T.decodeUtf8 (NaCl.encode x))


-- TODO register device token, get device description, revoke device, etc.
