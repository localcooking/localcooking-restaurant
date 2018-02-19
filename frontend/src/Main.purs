module Main where

import Spec (spec, initialState)

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Thermite as T


main :: forall e. Eff _ Unit
main = do
  log "Hello sailor!"
  T.defaultMain spec initialState unit
