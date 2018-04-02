module Spec.Content.Root where

import Window (WindowSize (Laptop), initWindowSize)

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
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.Icons.Search (searchIcon)
import MaterialUI.Icons.PictureInPicture (pictureInPictureIcon)
import MaterialUI.Icons.ShoppingCart (shoppingCartIcon)
import MaterialUI.Icons.Timelapse (timelapseIcon)
import MaterialUI.Icons.LocalShipping (localShippingIcon)

import IxSignal.Internal (IxSignal)



type State =
  { windowSize :: WindowSize
  }

initialState :: State
initialState =
  { windowSize: unsafePerformEff initWindowSize
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
      ] <> ( if state.windowSize < Laptop
                then paragraph1
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid
                      { xs: 9
                      , item: true
                      } $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph1 <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    ]
                  ]
           ) <>
      [ divider {}
      , typography
        { variant: if state.windowSize < Laptop then Typography.headline else Typography.display1
        , align: Typography.left
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em", marginTop: "1em"}
        } [R.text "Simplified Ordering"]
      , if state.windowSize < Laptop
          then paragraph2
          else
            grid
              { spacing: Grid.spacing8
              , container: true
              }
              [ grid {xs: 3, item: true} []
              , grid
                { xs: 9
                , item: true
                }
                [ R.div [RP.style {marginTop: "1em"}] []
                , paragraph2
                , R.div [RP.style {marginBottom: "1em"}] []
                ]
              ]
      , divider {}
      , typography
        { variant: if state.windowSize < Laptop then Typography.headline else Typography.display1
        , align: Typography.right
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em", marginTop: "1em"}
        } [R.text "How Long Does it Take, and Why?"]
      ] <> ( if state.windowSize < Laptop
                then paragraph3
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid
                      { xs: 9
                      , item: true
                      } $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph3 <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    ]
                  ]
           )


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


paragraph1 :: Array R.ReactElement
paragraph1 =
  [ typography
    { variant: Typography.body1
    , align: Typography.left
    }
    [ R.text "We are a team of chefs dedicated to providing hand-made, healthy, creative meals to the public at competitive prices. Our platform allows chefs to debut "
    , R.em [] [R.text "their own"]
    , R.text " menus and feature "
    , R.em [] [R.text "their own"]
    , R.text " culinary artistry - search for a specific dish, or for a style of talent."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    }
    [ R.text "Our chefs are paid by commission; they receive the majority of profit on every order, while our app gives them an opportunity to reach more customers, looking for their type of cuisine. We want to make the experience of ordering a hand-cooked meal"
    , R.em [] [R.text "personal"]
    , R.text ", yet "
    , R.em [] [R.text "streamlined"]
    , R.text " enough to meet the needs of our modern world."
    ]
  ]


-- FIXME links!!
paragraph2 :: R.ReactElement
paragraph2 = list {dense: true}
  [ listItem {}
    [ listItemIcon {} searchIcon
    , listItemText
      {primary: "Browse our Chefs and Menus"}
    ]
  , listItem {}
    [ listItemIcon {} pictureInPictureIcon
    , listItemText
      {primary: "View the details on specific meals - the ingredients, the culture and history, and how to prepare it"}
    ]
  , listItem {}
    [ listItemIcon {} shoppingCartIcon
    , listItemText
      {primary: "Create an order of at least $100, at least two weeks in advance"}
    ]
  , listItem {}
    [ listItemIcon {} timelapseIcon
    , listItemText
      {primary: "Checkout your cart, wait for updates on your order"}
    ]
  , listItem {}
    [ listItemIcon {} localShippingIcon
    , listItemText
      {primary: "Receive your delivery of frozen meals, store them, or prepare and enjoy!"}
    ]
  ]


paragraph3 :: Array R.ReactElement
paragraph3 =
  [ typography
    { variant: Typography.body1
    , align: Typography.left
    }
    [ R.text "Every chef has a "
    , R.em [] [R.text "bi-weekly"]
    , R.text " schedule, and every order must be filed at least two weeks in advance - each menu has its own shipping date, and each chef has their own planned schedule to fill their orders. This allows chefs to "
    , R.em [] [R.text "care"]
    , R.text " about each meal and give their full attention to their craft, without having to worry about wasteful time constraints."
    ]
  ]
