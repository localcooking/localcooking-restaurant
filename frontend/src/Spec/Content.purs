module Spec.Content where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Markdown (markdown)
import Control.Monad.Eff.Uncurried (mkEffFn2)

import MaterialUI.Types (createStyles)
import MaterialUI.Paper (paper)
import MaterialUI.Divider (divider)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Tabs (tabs, tab)
import MaterialUI.Tabs as Tabs


data Page
  = Home
  | Plan
  | Timeline


type State =
  { page :: Page
  }

initialState :: State
initialState =
  { page: Home
  }

data Action
  = ChangedPage Page


spec :: T.Spec _ State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedPage p -> void $ T.cotransform _ { page = p }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ R.main [RP.style {marginTop: "4.5em"}]
        [ paper
          { style: createStyles
            { maxWidth: "80em"
            , width: "100%"
            , marginLeft: "auto"
            , marginRight: "auto"
            , padding: "1em"
            }
          }
          [ tabs
            { value: case state.page of
                 Home -> 0
                 Plan -> 1
                 Timeline -> 2
            , onChange: mkEffFn2 \_ value -> dispatch $ ChangedPage $
                          if value == 0 then Home else if value == 1 then Plan else Timeline
            , indicatorColor: Tabs.secondary
            , textColor: Tabs.primary
            , centered: true
            }
            [ tab {label: R.text "Home"}
            , tab {label: R.text "Plan"}
            , tab {label: R.text "Timeline"}
            ]
          , paper {style: createStyles {minHeight: "30em"}}
            [ markdown
              { source: case state.page of
                   Home -> homeMd
                   Plan -> planMd
                   Timeline -> timelineMd
              , renderers:
                { heading: R.createClassStateless' \{level} children ->
                    typography
                      { variant:  if level == 1 then Typography.display4
                                  else if level == 2 then Typography.display3
                                  else if level == 3 then Typography.display2
                                  else if level == 4 then Typography.display1
                                  else if level == 5 then Typography.headline
                                  else Typography.subheading
                      , style: createStyles {marginLeft: "1em"}
                      } children
                , blockquote: R.createClassStateless' \_ children ->
                    typography
                      { variant: Typography.caption
                      } children
                , paragraph: R.createClassStateless' \_ children ->
                    typography
                      { variant: Typography.body1
                      , style: createStyles {margin: "1em", textIndent: "2em"}
                      } children
                , strong: R.createClassStateless' \_ children ->
                    typography
                      { variant: Typography.body2
                      } children
                , thematicBreak: R.createClassStateless' \_ children ->
                    divider {}
                }
              }
            ]
          , typography
            { variant: Typography.caption
            , style: createStyles {marginTop: "5em", textAlign: "center"}
            }
            [ R.text "Copyright © Local Cooking Inc. 2018, All rights reserved" ]
          ]
        ]
      ]



content :: R.ReactElement
content =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []


homeMd :: String
homeMd = """
Local Cooking
=============

Welcome to [LocalCooking.com](http://localcooking.com)

----------------------------------

This website is a repository for the Local Cooking ideas, business plan, timelines, and strategies.
It's currently under construction; please forward all complaints and concerns to [localcookinginc@gmail.com](mailto:localcookinginc@gmail.com).
"""

planMd :: String
planMd = """
Business Plan
=============

This page is currently under construction.
"""


timelineMd :: String
timelineMd = """
Timeline and Strategy
=====================

This page is currently under construction.
"""
