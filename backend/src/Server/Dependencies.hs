{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies where

import Server.Dependencies.AuthToken (authTokenServer)
import Server.Dependencies.Register (registerServer)
import Types (AppM)

import Web.Routes.Nested (RouterT, l_, o_, (</>))
import Web.Dependencies.Sparrow (Topic (..), Server, serveDependencies, unpackServer, SparrowServerT, match)
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
dependencies = do
  authTokenDep <- unpackServer (Topic ["authToken"]) authTokenServer
  registerDep <- unpackServer (Topic ["register"]) registerServer
  match (l_ "authToken" </> o_) authTokenDep
  match (l_ "register" </> o_) registerDep



servedDependencies :: AppM (RouterT (MiddlewareT AppM) sec AppM ())
servedDependencies = serveDependencies dependencies
