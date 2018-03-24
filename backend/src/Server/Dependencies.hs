{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies where

import Server.Dependencies.DeviceToken (deviceTokenServer)
import Types (AppM)

import Web.Routes.Nested (RouterT, l_, o_, (</>))
import Web.Dependencies.Sparrow (Topic (..), Server, serveDependencies, unpackServer, SparrowServerT, match)
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
dependencies = do
  deviceToken <- unpackServer (Topic ["device-token"]) deviceTokenServer
  match (l_ "device-token" </> o_) deviceToken



servedDependencies :: AppM (RouterT (MiddlewareT AppM) sec AppM ())
servedDependencies = serveDependencies dependencies
