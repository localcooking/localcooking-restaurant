module Spec.Content.Root where

import Window (WindowSize (Laptop), initialWindowSize)

import Prelude
import Data.UUID (GENUUID)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Markdown (markdown)
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid

import IxSignal.Internal (IxSignal)



type State =
  { windowSize :: WindowSize
  }

initialState :: State
initialState =
  { windowSize: unsafePerformEff initialWindowSize
  }

data Action
  = ChangedWindowSize WindowSize

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)


spec :: forall eff
      . T.Spec eff State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: if state.windowSize < Laptop then Typography.headline else Typography.display1
        , align: Typography.right
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em"}
        } [R.text "Locally Sourced, Affordable Cuisine for the Average Family"]
      , if state.windowSize < Laptop
          then markdown paragraph1
          else
            grid
              { spacing: Grid.spacing8
              , container: true
              }
              [ grid
                { xs: 9
                , item: true
                }
                [ R.div [RP.style {marginTop: "1em"}] []
                , markdown paragraph1
                , R.div [RP.style {marginBottom: "1em"}] []
                ]
              ]
      , divider {}
      , typography
        { variant: Typography.display1
        , align: Typography.left
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em", marginTop: "1em"}
        } [R.text "Simplified Ordering"]
      ]


root :: forall eff
      . { windowSizeSignal :: IxSignal (Effects eff) WindowSize
        }
     -> R.ReactElement
root {windowSizeSignal} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
      reactSpec' =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []


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
