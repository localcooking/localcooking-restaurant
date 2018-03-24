module Server.Dependencies.DeviceToken where

import Data.Text (Text)
import LocalCooking.Device (DeviceToken)


data DeviceTokenInitIn
  = DeviceTokenInitInNew
    { deviceTokenInitInNewName :: Text
    }
  | DeviceTokenInitInVerify DeviceToken


data DeviceTokenInitOut
  = DeviceTokenInitOut DeviceToken


data DeviceTokenDeltaIn


data DeviceTokenDeltaOut
  = DeviceTokenDeltaOutRevoked
