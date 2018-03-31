module Spec.Topbar where

import Links (toLocation, SiteLinks (..), initSiteLinks, LogoLinks (..))
import Window (WindowSize (..), initialWindowSize)

import Prelude
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
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.AppBar (appBar)
import MaterialUI.AppBar as AppBar
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.IconButton (iconButton)
import MaterialUI.IconButton as IconButton
import MaterialUI.Icons.Menu (menuIcon)

import Queue.One (WRITE, Queue, putQueue)
import IxSignal.Internal (IxSignal)
import Unsafe.Coerce (unsafeCoerce)



type State =
  { windowSize :: WindowSize
  , currentPage :: SiteLinks
  }

initialState :: State
initialState =
  { windowSize: unsafePerformEff initialWindowSize
  , currentPage: initSiteLinks
  }

data Action
  = OpenLogin
  | ClickedMobileMenuButton
  | ChangedWindowSize WindowSize
  | ChangedCurrentPage SiteLinks
  | ClickedAboutLink
  | ClickedMealsLink
  | ClickedChefsLink
  | ClickedLogo

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


spec :: forall eff
      . { toURI :: Location -> URI
        , openLoginSignal :: Queue (write :: WRITE) (Effects eff) Unit
        , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
        , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  { toURI
  , openLoginSignal
  , siteLinks
  , mobileMenuButtonSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      OpenLogin -> liftEff (putQueue openLoginSignal unit)
      ClickedMobileMenuButton -> liftEff (putQueue mobileMenuButtonSignal unit)
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedCurrentPage x -> void $ T.cotransform _ { currentPage = x }
      ClickedAboutLink -> liftEff (siteLinks AboutLink)
      ClickedMealsLink -> liftEff (siteLinks MealsLink)
      ClickedChefsLink -> liftEff (siteLinks ChefsLink)
      ClickedLogo -> liftEff (siteLinks RootLink)

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
              [ R.a [ RP.href $ URI.print $ toURI $ toLocation RootLink
                    , RP.onClick \e -> do
                        (_ :: Unit) <- (unsafeCoerce e).stopPropagation
                        (_ :: Unit) <- (unsafeCoerce e).nativeEvent.stopImmediatePropagation
                        dispatch ClickedLogo
                    ]
                 [ R.img  [ RP.src $ URI.print $ toURI $ toLocation Logo40Png
                          , RP.style {height: "2.5em", border: 0}
                          ] [] -- FIXME make into a link to RootLink
                 ]
              , button
                { color: Button.inherit
                , disabled: state.currentPage == AboutLink
                , onTouchTap: mkEffFn1 \_ -> dispatch ClickedAboutLink
                } [R.text "About"]
              , button
                { color: Button.primary
                , variant: Button.raised
                , disabled: state.currentPage == MealsLink
                , onTouchTap: mkEffFn1 \_ -> dispatch ClickedMealsLink
                } [R.text "Meals"]
              , button
                { color: Button.secondary
                , variant: Button.raised
                , disabled: state.currentPage == ChefsLink
                , onTouchTap: mkEffFn1 \_ -> dispatch ClickedChefsLink
                } [R.text "Chefs"]
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
          , openLoginSignal :: Queue (write :: WRITE) (Effects eff) Unit
          , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
          , currentPageSignal :: IxSignal (Effects eff) SiteLinks
          , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
          } -> R.ReactElement
topbar
  { toURI
  , openLoginSignal
  , windowSizeSignal
  , siteLinks
  , mobileMenuButtonSignal
  , currentPageSignal
  } =
  let {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          { toURI
          , openLoginSignal
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
