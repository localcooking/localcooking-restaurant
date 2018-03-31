module Spec.Content.Root where

import Prelude

import Thermite as T
import React as R
import React.DOM as R

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Markdown (markdown)
import MaterialUI.Divider (divider)


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
      [ typography
        { variant: Typography.display1
        , align: Typography.right
        , color: Typography.primary
        } [R.text "Locally Sourced, Affordable Cuisine for the Average Family"]
      , markdown paragraph1
      ]


root :: R.ReactElement
root =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []


paragraph1 :: String
paragraph1 = """
We are a team of chefs dedicated to providing hand-made, healthy, creative meals to
the public at competitive prices. Our platform allows chefs to debut **their own** menus
and feature **their own** culinary artistry - search for a specific dish, or for a style of talent.

Our chefs are paid by commission; they receive the majority of profit on every order, while our
app gives them an opportunity to reach more customers, looking for their type of cuisine.
We want to make the experience of ordering a hand-cooked meal _personal_ again, yet
_streamlined_ enough to meet the needs of our modern world.
"""
