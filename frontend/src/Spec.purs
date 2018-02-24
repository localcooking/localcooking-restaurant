module Spec where

import Spec.Topbar (topbar)
import Spec.Content (content)
import Spec.Dialogs.Login (loginDialog)
import Colors (palette)
import Window (WindowSize)

import Prelude
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import MaterialUI.MuiThemeProvider (muiThemeProvider, createMuiTheme)
import MaterialUI.Reboot (reboot)

import Queue.One (newQueue, readOnly, writeOnly)
import IxSignal.Internal (IxSignal)



type State = Unit

initialState :: State
initialState = unit

type Action = Unit

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)

spec :: forall eff
      . { toURI :: Location -> URI
        , windowSizeSignal :: IxSignal (Effects eff) WindowSize
        }
     -> T.Spec (Effects eff) State Unit Action
spec {toURI,windowSizeSignal} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children = template
      [ topbar
        { toURI
        , openSignal: writeOnly openSignal
        , windowSizeSignal
        }
      , content
      , loginDialog
        { openSignal: readOnly openSignal
        }
      ]
      where
        template content =
          [ reboot
          , muiThemeProvider
              { theme: createMuiTheme {palette}
              }
              (R.div [] content)
          ]

        openSignal = unsafePerformEff newQueue
