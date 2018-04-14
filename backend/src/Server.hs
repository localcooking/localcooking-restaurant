{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , QuasiQuotes
  , DataKinds
  #-}

module Server where

import Server.HTTP (httpServer)
import Server.Dependencies (dependencies)
import Server.Assets (favicons, frontend, frontendMin)
import Links (SiteLinks)

import LocalCooking.Server (LocalCookingArgs (..))
import LocalCooking.Colors (LocalCookingColors (..))

import Text.Lucius (Color (..))


server :: LocalCookingArgs SiteLinks sec
server = LocalCookingArgs
  { localCookingArgsFrontend = frontend
  , localCookingArgsFrontendMin = frontendMin
  , localCookingArgsFavicons = favicons
  , localCookingArgsHTTP = httpServer
  , localCookingArgsDeps = dependencies
  , localCookingArgsColors = LocalCookingColors
    { localCookingColorsMain = Color 0 51 0
    , localCookingColorsActive = Color 27 94 32
    , localCookingColorsHover = Color 76 140 74
    }
  }
