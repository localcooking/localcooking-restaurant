{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies.UserDetails where

import Server.Dependencies.UserDetails.Email (userDetailsEmailServer)

import Types (AppM)

import Web.Routes.Nested (RouterT, l_, o_, (</>))
import Web.Dependencies.Sparrow (Topic (..), Server, serveDependencies, unpackServer, SparrowServerT, match)
import Network.Wai.Trans (MiddlewareT)



userDetailsDependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
userDetailsDependencies = do
  match (l_ "email" </> o_) =<< unpackServer (Topic ["userDetails","email"]) userDetailsEmailServer
