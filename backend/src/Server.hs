{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , QuasiQuotes
  , DataKinds
  #-}

module Server where

import Server.HTTP (httpServer)
import Server.Assets (favicons, frontend, frontendMin)
import Links (SiteLinks)

import LocalCooking.Server (LocalCookingArgs (..))
import LocalCooking.Colors (LocalCookingColors (..))
import LocalCooking.Dependencies.Restaurant (restaurantDependencies)

import Text.Lucius (Color (..))


server :: LocalCookingArgs SiteLinks sec
server = LocalCookingArgs
  { localCookingArgsFrontend = frontend
  , localCookingArgsFrontendMin = frontendMin
  , localCookingArgsFavicons = favicons
  , localCookingArgsHTTP = httpServer
  , localCookingArgsDeps = restaurantDependencies
  , localCookingArgsColors = LocalCookingColors
    { localCookingColorsMain = Color 74 0 114
    , localCookingColorsActive = Color 123 31 162
    , localCookingColorsHover = Color 174 82 212
    }
  }
