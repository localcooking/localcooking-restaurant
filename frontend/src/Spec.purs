module Spec where

import Spec.Topbar (topbar)
import Spec.Content (content)
import Spec.Dialogs.Login (loginDialog)
import Spec.Drawers.LeftMenu (leftMenu)
import Colors (palette)
import Window (WindowSize)
import Page (Page)
import Links (SiteLinks)

import Prelude
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)

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
  , now :: NOW
  | eff)

spec :: forall eff
      . { toURI :: Location -> URI
        , windowSizeSignal :: IxSignal (Effects eff) WindowSize
        , currentPageSignal :: IxSignal (Effects eff) Page
        , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
        , development :: Boolean
        }
     -> T.Spec (Effects eff) State Unit Action
spec {toURI,windowSizeSignal,siteLinks,currentPageSignal,development} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children = template
      [ topbar
        { toURI
        , openLoginSignal: writeOnly openLoginSignal
        , windowSizeSignal
        , siteLinks
        , mobileMenuButtonSignal: writeOnly mobileMenuButtonSignal
        , currentPageSignal
        }
      , content
        { currentPageSignal
        }
      , loginDialog
        { openLoginSignal: readOnly openLoginSignal
        , windowSizeSignal
        , toURI
        }
      , leftMenu
        { mobileDrawerOpenSignal: readOnly mobileMenuButtonSignal
        , siteLinks
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

        openLoginSignal = unsafePerformEff newQueue
        mobileMenuButtonSignal = unsafePerformEff newQueue



app :: forall eff
     . { toURI :: Location -> URI
       , windowSizeSignal :: IxSignal (Effects eff) WindowSize
       , currentPageSignal :: IxSignal (Effects eff) Page
       , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
       , development :: Boolean
       }
    -> { spec :: R.ReactSpec Unit State (Array R.ReactElement) (Effects eff)
       , dispatcher :: R.ReactThis Unit State -> Action -> T.EventHandler
       }
app {toURI,windowSizeSignal,currentPageSignal,siteLinks,development} =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec (spec {toURI,windowSizeSignal,currentPageSignal,siteLinks,development}) initialState

  in  {spec: reactSpec, dispatcher}
