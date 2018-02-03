module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM (render)


type State = Unit

initialState :: State
initialState = unit

type Action = Unit

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where
    performAction _ _ _ = pure unit

    render :: T.Render State _ Action
    render _ _ _ _ = [ R.div [] [ R.text "Yo" ] ]



main :: forall e. Eff _ Unit
main = do
  log "Hello sailor!"
  T.defaultMain spec initialState unit
