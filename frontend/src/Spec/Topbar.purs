module Spec.Topbar where

import Links (toLocation, SiteLinks (..), ImageLinks (..))
import Window (WindowSize (..))

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Text.Email.Validate (EmailAddress)
import Text.Email.Validate as Email
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
import React.DOM.Props.PreventDefault (preventDefault)
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
import IxSignal.Internal as IxSignal
import Partial.Unsafe (unsafePartial)



type State =
  { windowSize :: WindowSize
  , currentPage :: SiteLinks
  , userDetails :: Maybe {email :: EmailAddress}
  }

initialState :: {initWindowSize :: WindowSize, initSiteLinks :: SiteLinks} -> State
initialState {initWindowSize,initSiteLinks} =
  { windowSize: initWindowSize
  , currentPage: initSiteLinks
  , userDetails: Nothing
  }

data Action
  = OpenLogin
  | ClickedMobileMenuButton
  | ChangedWindowSize WindowSize
  | ChangedCurrentPage SiteLinks
  | ChangedUserDetails (Maybe {email :: EmailAddress})
  | Clicked SiteLinks

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
      ChangedUserDetails x -> void $ T.cotransform _ { userDetails = x }
      Clicked x -> liftEff (siteLinks x)

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
              let mkButton x = button
                    { color: unsafePartial $ case x of
                         RootLink -> Button.inherit
                         MealsLink -> Button.primary
                         ChefsLink -> Button.secondary
                    , disabled: state.currentPage == x
                    , onClick: mkEffFn1 preventDefault
                    , onTouchTap: mkEffFn1 \e -> do
                        preventDefault e
                        dispatch (Clicked x)
                    , href: URI.print $ toURI $ toLocation x
                    , variant: case x of
                      RootLink -> Button.flat
                      _ -> Button.raised
                    } [ R.text $ unsafePartial $ case x of
                          RootLink -> "About"
                          MealsLink -> "Meals"
                          ChefsLink -> "Chefs"
                      ]
              in  [ R.img  [ RP.src $ URI.print $ toURI $ toLocation Logo40Png
                          , RP.style {height: "2.5em", border: 0}
                          ] []
                  , mkButton RootLink
                  , mkButton MealsLink
                  , mkButton ChefsLink
                  ]
          ) <>
          [ R.div [RP.style {flex: 1, display: "flex", flexDirection: "row-reverse"}] $ case state.userDetails of
               Nothing ->
                [ button
                  { color: Button.inherit
                  , onTouchTap: mkEffFn1 \_ -> dispatch OpenLogin
                  } [R.text "Login"]
                ]
               Just {email} ->
                [ button
                  { color: Button.inherit
                  } [R.text $ Email.toString email]
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
          , userDetailsSignal :: IxSignal (Effects eff) (Maybe {email :: EmailAddress})
          } -> R.ReactElement
topbar
  { toURI
  , openLoginSignal
  , windowSizeSignal
  , siteLinks
  , mobileMenuButtonSignal
  , currentPageSignal
  , userDetailsSignal
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          { toURI
          , openLoginSignal
          , siteLinks
          , mobileMenuButtonSignal
          }
        )
        (initialState init)
      reactSpec' =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
        $ Signal.whileMountedIxUUID
            userDetailsSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedUserDetails x))
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
