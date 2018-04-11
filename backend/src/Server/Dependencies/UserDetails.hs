{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies.UserDetails where

import Server.Dependencies.UserDetails.Email (userDetailsEmailServer)

import LocalCooking.Types (AppM)

import Web.Routes.Nested (l_, o_, (</>))
import Web.Dependencies.Sparrow (Topic (..), unpackServer, SparrowServerT, match)
import Network.Wai.Trans (MiddlewareT)



userDetailsDependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
userDetailsDependencies = do
  match (l_ "email" </> o_) =<< unpackServer (Topic ["userDetails","email"]) userDetailsEmailServer
