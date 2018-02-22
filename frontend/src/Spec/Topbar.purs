module Spec.Topbar where

import Links (toLocation, LogoLinks (Logo40Png))

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)

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



type State = Unit

initialState :: State
initialState = unit

type Action = Unit

spec :: forall eff
      . { toURI :: Location -> URI
        }
     -> T.Spec eff State Unit Action
spec {toURI} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ appBar {color: AppBar.default, position: AppBar.static}
        [ toolbar {style: createStyles {display: "flex"}}
          -- [ typography
          --     { "type": Typography.title
          --     , color: Typography.inheritColor
          --     } [R.text "Local Cooking"]
          [ R.img [ RP.src $ URI.print $ toURI $ toLocation Logo40Png
                  , RP.style {height: "2.5em"}
                  ] []
          , button
            { color: Button.inherit
            , disabled: true
            } [R.text "About"]
          , button
            { color: Button.inherit
            } [R.text "Menu"]
          , R.div [RP.style {flex: 1, display: "flex", flexDirection: "row-reverse"}]
            [ button
              { color: Button.inherit
              } [R.text "Login"]
            ]
          ]
        ]
      ]



topbar :: { toURI :: Location -> URI
          } -> R.ReactElement
topbar params =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec params) initialState
  in  R.createElement (R.createClass reactSpec) unit []
