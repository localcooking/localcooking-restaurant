module Spec.Topbar where

import Links (toLocation, LogoLinks (..))

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP

import MaterialUI.Types (createStyles)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.AppBar (appBar)
import MaterialUI.AppBar as AppBar
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button

import Queue.One (WRITE, Queue, putQueue)



type State = Unit

initialState :: State
initialState = unit

data Action
  = OpenLogin

type Effects eff =
  ( ref :: REF
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

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ appBar {color: AppBar.default, position: AppBar.fixed}
        [ toolbar {style: createStyles {display: "flex"}}
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
          , R.div [RP.style {flex: 1, display: "flex", flexDirection: "row-reverse"}]
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
          } -> R.ReactElement
topbar params =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec params) initialState
  in  R.createElement (R.createClass reactSpec) unit []
