module Spec.Topbar where

import Links (toLocation, LogoLinks (..))
import Window (WindowSize (..))

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
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



type State =
  { windowSize :: WindowSize
  }

initialState :: State
initialState =
  { windowSize: Pager
  }

data Action
  = OpenLogin
  | ChangedWindowSize WindowSize

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


spec :: forall eff
      . { toURI :: Location -> URI
        , openSignal :: Queue (write :: WRITE) (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec {toURI,openSignal} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      OpenLogin -> liftEff (putQueue openSignal unit)
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ appBar {color: AppBar.default, position: AppBar.fixed}
        [ toolbar {style: createStyles {display: "flex"}} $
          ( if state.windowSize < Laptop
            then
              [ iconButton
                { color: IconButton.inherit
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
              , onTouchTap: mkEffFn1 \_ -> dispatch OpenLogin
              } [R.text "Login"]
            ]
          ]
        ]
      ]



topbar :: forall eff
        . { toURI :: Location -> URI
          , openSignal :: Queue (write :: WRITE) (Effects eff) Unit
          , windowSizeSignal :: IxSignal (Effects eff) WindowSize
          } -> R.ReactElement
topbar {toURI,openSignal,windowSizeSignal} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec {toURI,openSignal}) initialState
      reactSpec' = whileMountedIxUUID windowSizeSignal (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
                   reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
