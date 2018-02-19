module Spec.Topbar where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP

import MaterialUI.Toolbar (toolbar)
import MaterialUI.AppBar (appBar)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography



type State = Unit

initialState :: State
initialState = unit

type Action = Unit

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where
    performAction _ _ _ = pure unit

    render :: T.Render State _ Action
    render _ _ _ _ =
      [ appBar {}
        [ toolbar {}
          [ typography
              { "type": Typography.title
              , color: Typography.inheritColor
              } [R.text "Local Cooking"]
          ]
        ]
      ]



topbar :: R.ReactElement
topbar =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []
