module Spec.Content.UserDetails where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP

import MaterialUI.Types (createStyles)
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)



type State = Unit

initialState :: State
initialState = unit

type Action = Unit


spec :: forall eff. T.Spec eff State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ Drawer.withStyles
        (\_ -> {paper: createStyles {position: "relative", width: "200px", zIndex: 1000}})
        \{classes} -> drawer
          { variant: Drawer.permanent
          , anchor: Drawer.left
          , classes: Drawer.createClasses classes
          }
          [ list {dense: true}
            [ listItem {}
              [ listItemText
                { primary: "General"
                }
              ]
            ]
          ]
      , R.div [RP.style {position: "absolute", left: "200px"}]
        [ R.text "UserDetails"
        ]
      ]


userDetails :: R.ReactElement
userDetails =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []
