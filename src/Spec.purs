module Spec where

import Spec.Topbar (topbar)
import Spec.Content (content)

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import MaterialUI.MuiThemeProvider (muiThemeProvider, createMuiTheme)




type State = Unit

initialState :: State
initialState = unit

type Action = Unit

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where
    performAction _ _ _ = pure unit

    render :: T.Render State _ Action
    render _ _ _ _ = template
      [ topbar
      , content
      ]
      where
        template content =
          [ muiThemeProvider {theme: createMuiTheme unit}
              (R.div [] content)
          ]
