module Spec where

import Spec.Topbar (topbar)
import Spec.Content (content)
import Spec.Dialogs.Login (loginDialog)
import Colors (palette)
import Window (WindowSize)
import Links (SiteLinks (..))

import Prelude
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import MaterialUI.MuiThemeProvider (muiThemeProvider, createMuiTheme)
import MaterialUI.Reboot (reboot)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)

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
  , dom :: DOM
  , history :: HISTORY
  | eff)

spec :: forall eff
      . { toURI :: Location -> URI
        , windowSizeSignal :: IxSignal (Effects eff) WindowSize
        , currentPageSignal :: IxSignal (Effects eff) SiteLinks
        , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec {toURI,windowSizeSignal,siteLinks,currentPageSignal} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children = template
      [ topbar
        { toURI
        , openSignal: writeOnly openSignal
        , windowSizeSignal
        , siteLinks
        }
      , content
        { currentPageSignal
        }
      , loginDialog
        { openSignal: readOnly openSignal
        , windowSizeSignal
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



app :: forall eff
     . { toURI :: Location -> URI
       , windowSizeSignal :: IxSignal (Effects eff) WindowSize
       , currentPageSignal :: IxSignal (Effects eff) SiteLinks
       , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
       }
    -> { spec :: R.ReactSpec Unit State (Array R.ReactElement) (Effects eff)
       , dispatcher :: R.ReactThis Unit State -> Action -> T.EventHandler
       }
app {toURI,windowSizeSignal,currentPageSignal,siteLinks} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec (spec {toURI,windowSizeSignal,currentPageSignal,siteLinks}) initialState

  in  {spec: reactSpec, dispatcher}
