module Spec.Topbar where

import Links (toLocation, SiteLinks (..), LogoLinks (..))
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

import Thermite as T
import Thermite.Window as WindowT
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted (whileMountedIxUUID)

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



type State = Unit

initialState :: State
initialState = unit

data Action
  = OpenLogin
  | ClickedMobileMenuButton

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
     -> T.Spec (Effects eff) (WindowT.State State) Unit (WindowT.Action Action)
spec
  { toURI
  , openSignal
  , siteLinks
  , mobileMenuButtonSignal
  } = T.simpleSpec (WindowT.performAction performAction) render
  where
    performAction action props state = case action of
      OpenLogin -> liftEff (putQueue openSignal unit)
      ClickedMobileMenuButton -> liftEff (putQueue mobileMenuButtonSignal unit)

    render :: T.Render (WindowT.State State) Unit (WindowT.Action Action)
    render dispatch props {windowSize,state} children =
      [ appBar {color: AppBar.default, position: AppBar.fixed}
        [ toolbar {style: createStyles {display: "flex"}} $
          ( if windowSize < Laptop
            then
              [ iconButton
                { color: IconButton.inherit
                , onTouchTap: mkEffFn1 \_ -> dispatch $ WindowT.Action ClickedMobileMenuButton
                } menuIcon
              ]
            else
              [ R.img [ RP.src $ URI.print $ toURI $ toLocation Logo40Png
                      , RP.style {height: "2.5em"}
                      ] []
              , button
                { color: Button.inherit
                , disabled: true
                } [R.text "About"]
              , button
                { color: Button.primary
                , variant: Button.raised
                } [R.text "Menu"]
              ]
          ) <>
          [ R.div [RP.style {flex: 1, display: "flex", flexDirection: "row-reverse"}]
            [ button
              { color: Button.inherit
              , onTouchTap: mkEffFn1 \_ -> dispatch $ WindowT.Action OpenLogin
              } [R.text "Login"]
            ]
          ]
        ]
      ]



topbar :: forall eff
        . { toURI :: Location -> URI
          , openSignal :: Queue (write :: WRITE) (Effects eff) Unit
          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
          , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
          , mobileMenuButtonSignal :: Queue (write :: WRITE) (Effects eff) Unit
          } -> R.ReactElement
topbar {toURI,openSignal,windowSizeSignal,siteLinks,mobileMenuButtonSignal} =
  let {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          { toURI
          , openSignal
          , siteLinks
          , mobileMenuButtonSignal
          }
        ) (WindowT.initialState initialState)
      reactSpec' = WindowT.listening windowSizeSignal {spec:reactSpec,dispatcher}
  in  R.createElement (R.createClass reactSpec') unit []
