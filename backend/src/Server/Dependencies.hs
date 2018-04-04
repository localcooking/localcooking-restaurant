{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies where

import Server.Dependencies.AuthToken (authTokenServer)
import Server.Dependencies.Register (registerServer)
import Server.Dependencies.UserDetails (userDetailsDependencies)

import Types (AppM)

import Web.Routes.Nested (RouterT, l_, o_, (</>))
import Web.Dependencies.Sparrow (Topic (..), Server, serveDependencies, unpackServer, SparrowServerT, match, matchGroup)
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
dependencies = do
  match (l_ "authToken" </> o_) =<< unpackServer (Topic ["authToken"]) authTokenServer
  match (l_ "register" </> o_) =<< unpackServer (Topic ["register"]) registerServer
  matchGroup (l_ "userDetails" </> o_) userDetailsDependencies



servedDependencies :: AppM (RouterT (MiddlewareT AppM) sec AppM ())
servedDependencies = serveDependencies dependencies
