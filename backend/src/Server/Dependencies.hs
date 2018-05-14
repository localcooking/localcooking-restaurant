{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies where

import LocalCooking.Types (AppM)


import Web.Routes.Nested (l_, o_, (</>))
import Web.Dependencies.Sparrow (SparrowServerT, matchGroup)
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT AppM) [] AppM ()
dependencies =
  pure ()
