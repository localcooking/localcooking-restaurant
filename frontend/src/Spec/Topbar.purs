module Spec.Topbar where

import Links (toLocation, SiteLinks (..), LogoLinks (..))
import Page (Page (..), initPage)
import Window (WindowSize (..))

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.AppBar (appBar)
import MaterialUI.AppBar as AppBar
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.IconButton (iconButton)
import MaterialUI.IconButton as IconButton
import MaterialUI.Icons.Menu (menuIcon)

import Queue.One (WRITE, Queue, putQueue)
import IxSignal.Internal (IxSignal)



type State =
  { windowSize :: WindowSize
  , currentPage :: Page
  }

initialState :: State
initialState =
  { windowSize: Pager
  , currentPage: initPage
  }

data Action
  = OpenLogin
  | ClickedMobileMenuButton
  | ChangedWindowSize WindowSize
  | ChangedCurrentPage Page
  | ClickedAboutLink
  | ClickedMenuLink

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


spec :: forall eff
      . { toURI :: Location -> URI
        , openSignal :: Queue (write :: WRITE) (Effects eff) Unit
        , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
        , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  { toURI
  , openSignal
  , siteLinks
  , mobileMenuButtonSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      OpenLogin -> liftEff (putQueue openSignal unit)
      ClickedMobileMenuButton -> liftEff (putQueue mobileMenuButtonSignal unit)
      ChangedWindowSize w -> do
        liftEff $ unsafeCoerceEff $ log $ "Uh... window size: " <> show w
        void $ T.cotransform _ { windowSize = w }
      ChangedCurrentPage x -> void $ T.cotransform _ { currentPage = x }
      ClickedAboutLink -> liftEff (siteLinks AboutLink)
      ClickedMenuLink -> liftEff (siteLinks RootLink)

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ appBar {color: AppBar.default, position: AppBar.fixed}
        [ toolbar {style: createStyles {display: "flex"}} $
          ( if state.windowSize < Laptop
            then
              [ iconButton
                { color: IconButton.inherit
                , onTouchTap: mkEffFn1 \_ -> dispatch ClickedMobileMenuButton
                } menuIcon
              ]
            else
              [ R.img [ RP.src $ URI.print $ toURI $ toLocation Logo40Png
                      , RP.style {height: "2.5em"}
                      ] []
              , button
                { color: Button.inherit
                , disabled: state.currentPage == AboutPage
                , onTouchTap: mkEffFn1 \_ -> dispatch ClickedAboutLink
                } [R.text "About"]
              , button
                { color: Button.primary
                , variant: Button.raised
                , disabled: state.currentPage == MenuPage
                , onTouchTap: mkEffFn1 \_ -> dispatch ClickedMenuLink
                } [R.text "Menu"]
              ]
          ) <>
          [ R.div [RP.style {flex: 1, display: "flex", flexDirection: "row-reverse"}]
            [ button
              { color: Button.inherit
              , onTouchTap: mkEffFn1 \_ -> dispatch OpenLogin
              } [R.text "Login"]
            ]
          ]
        ]
      ]



topbar :: forall eff
        . { toURI :: Location -> URI
          , openSignal :: Queue (write :: WRITE) (Effects eff) Unit
          , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
          , currentPageSignal :: IxSignal (Effects eff) Page
          , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
          } -> R.ReactElement
topbar
  { toURI
  , openSignal
  , windowSizeSignal
  , siteLinks
  , mobileMenuButtonSignal
  , currentPageSignal
  } =
  let {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          { toURI
          , openSignal
          , siteLinks
          , mobileMenuButtonSignal
          }
        ) initialState
      reactSpec' =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
