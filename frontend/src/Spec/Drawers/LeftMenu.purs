module Spec.Drawers.LeftMenu where

import Window (WindowSize, initialWindowSize)
import Links (SiteLinks (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal
import React.Icons (facebookIcon, twitterIcon, googleIcon)

import MaterialUI.Types (createStyles)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.Divider (divider)
import MaterialUI.Icons.PersonPin (personPinIcon)
import MaterialUI.Icons.RestaurantMenu (restaurantMenuIcon)

import Queue.One (WRITE, READ, Queue, putQueue)
import IxSignal.Internal (IxSignal)


type State =
  { open :: Boolean
  , windowSize :: WindowSize
  }

initialState :: State
initialState =
  { open: false
  , windowSize: unsafePerformEff initialWindowSize
  }

data Action
  = ChangedWindowSize WindowSize
  | ClickedAboutLink
  | ClickedMenuLink
  | Open
  | Close


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


spec :: forall eff
      . { siteLinks :: SiteLinks -> Eff (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec {siteLinks} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ClickedAboutLink -> do
        performAction Close props state
        liftEff (siteLinks AboutLink)
      ClickedMenuLink -> do
        performAction Close props state
        liftEff (siteLinks RootLink)
      Open -> void $ T.cotransform _ { open = true }
      Close -> void $ T.cotransform _ { open = false }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ drawer
        { open: state.open
        , onClose: mkEffFn1 \_ -> dispatch Close
        }
        [ list {}
          [ listItem
            { button: true
            , onClick: mkEffFn1 \_ -> dispatch ClickedMenuLink
            }
            [ listItemIcon {} restaurantMenuIcon
            , listItemText {primary: "Menu"}
            ]
          , divider {}
          , listItem
            { button: true
            , onClick: mkEffFn1 \_ -> dispatch ClickedAboutLink
            }
            [ listItemIcon {} personPinIcon
            , listItemText {primary: "About"}
            ]
          ]
        ]
      ]


leftMenu :: forall eff
          . { mobileDrawerOpenSignal :: Queue (read :: READ) (Effects eff) Unit
            , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
            }
         -> R.ReactElement
leftMenu
  { mobileDrawerOpenSignal
  , siteLinks
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            {siteLinks}
          ) initialState
      reactSpecLogin =
          Queue.whileMountedOne
            mobileDrawerOpenSignal
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
            reactSpec
  in  R.createElement (R.createClass reactSpecLogin) unit []
