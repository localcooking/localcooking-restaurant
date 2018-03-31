module Login.Storage where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Argonaut (jsonParser, decodeJson, encodeJson)
import Control.Monad.Eff (Eff)
import LocalCooking.Common.AuthToken (AuthToken)

import Browser.WebStorage (WEB_STORAGE, localStorage, getItem, setItem, removeItem)


getStoredAuthToken :: forall eff. Eff (webStorage :: WEB_STORAGE | eff) (Maybe AuthToken)
getStoredAuthToken = do
  mString <- getItem localStorage "authToken"
  case mString of
    Nothing -> pure Nothing
    Just string -> case jsonParser string >>= decodeJson of
      Left e -> pure Nothing
      Right x -> pure (Just x)


storeAuthToken :: forall eff. AuthToken -> Eff (webStorage :: WEB_STORAGE | eff) Unit
storeAuthToken authToken =
  setItem localStorage "authToken" $ show $ encodeJson authToken


clearAuthToken :: forall eff. Eff (webStorage :: WEB_STORAGE | eff) Unit
clearAuthToken =
  removeItem localStorage "authToken"
