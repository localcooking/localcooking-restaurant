module Spec.Content.Root where

import Links (SiteLinks, AboutPageLinks (..))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)

import Prelude
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.URI as URI
import Data.URI.Location (Location, toLocation)
import Data.Lens (Lens', Prism', lens, prism')
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (div, em, img, strong, text) as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import DOM.HTML.Window.Extra (WindowSize (Laptop))

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
import MaterialUI.Icons.RestaurantMenu (restaurantMenuIcon)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State =
  { localCooking :: LocalCookingState SiteLinks UserDetails
  }

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingAction (LocalCookingAction SiteLinks UserDetails)

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)

getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> T.Spec (Effects eff) State Unit Action
spec params@{toURI} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: if state.localCooking.windowSize < Laptop
                      then Typography.headline
                      else Typography.display1
        , align: Typography.right
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em"}
        } [R.text "Build Your Own Menus, Sell Your Own Creations"]
      ] <> ( if state.localCooking.windowSize < Laptop
                then paragraph1
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid {xs: 8, item: true} $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph1 <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    , grid {xs: 4, item: true}
                      [ R.img
                        [ RP.src $ URI.print $ toURI $ toLocation Paragraph1Png
                        , RP.style {width: "100%", marginBottom: "1em", borderRadius: "0 0.2em 0.2em 0"}
                        ] []
                      ]
                    ]
                  ]
           ) <>
      [ divider {}
      , typography
        { variant: if state.localCooking.windowSize < Laptop
                      then Typography.headline
                      else Typography.display1
        , align: Typography.left
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em", marginTop: "1em"}
        } [R.text "Work Your Own Schedule, Manage Your Own Orders"]
      , if state.localCooking.windowSize < Laptop
          then paragraph2
          else
            grid
              { spacing: Grid.spacing8
              , container: true
              }
              [ grid {xs: 4, item: true}
                [ R.img
                  [ RP.src $ URI.print $ toURI $ toLocation Paragraph2Png
                  , RP.style {width: "100%", marginBottom: "1em", borderRadius: "0.2em 0 0 0.2em"}
                  ] []
                ]
              , grid {xs: 8, item: true}
                [ R.div [RP.style {marginTop: "1em"}] []
                , paragraph2
                , R.div [RP.style {marginBottom: "1em"}] []
                ]
              ]
      , divider {}
      , typography
        { variant: if state.localCooking.windowSize < Laptop
                      then Typography.headline
                      else Typography.display1
        , align: Typography.right
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em", marginTop: "1em"}
        } [R.text "Develop Your Own Portfolio And Local Reputation"]
      ] <> ( if state.localCooking.windowSize < Laptop
                then paragraph3
                else
                  [ grid
                    { spacing: Grid.spacing8
                    , container: true
                    }
                    [ grid
                      { xs: 8
                      , item: true
                      } $
                      [ R.div [RP.style {marginTop: "1em"}] []
                      ] <> paragraph3 <>
                      [ R.div [RP.style {marginBottom: "1em"}] []
                      ]
                    , grid {xs: 4, item: true}
                      [ R.img
                        [ RP.src $ URI.print $ toURI $ toLocation Paragraph3Png
                        , RP.style {width: "100%", marginBottom: "1em", borderRadius: "0 0.2em 0.2em 0"}
                        ] []
                      ]
                    ]
                  ]
           )


root :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> R.ReactElement
root params =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
        whileMountedLocalCooking
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []


paragraph1 :: Array R.ReactElement
paragraph1 =
  [ typography
    { variant: Typography.body1
    , align: Typography.left
    , paragraph: true
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "Building and managing a website is hard, but in our modern age, it's a necessary requirement for any serious small business or craftsman. Our platform takes care of the technical details by giving our chefs an "
    , R.em [] [R.text "interactive menu"]
    , R.text " editing system; each chef crafts "
    , R.strong [] [R.text "their own"]
    , R.text " menus and recipes, using ingredients locally available. — search for a specific dish, or for a style of talent."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "Every menu is modern and richly interactive — chefs can publish a near unlimited amount of detail per meal, giving rise to various multimedia accompanied with each meal description — from close-ups of a finished product, to preparation instructions, we have an open playing field for our chefs to express their talent."
    ]
  , typography
    { variant: Typography.body1
    , align: Typography.left
    , style: createStyles {textIndent: "3em"}
    }
    [ R.text "Local Cooking chefs are independent contractors, and get paid a majority commission per-order; Local Cooking just manages the insurance of wholesale produce orders and kitchen supplies & upkeep.Each chef is responsible for their customer base and supplying market demands."
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
      {primary: "View the details on specific meals — the ingredients, the culture and history, and how to prepare it"}
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
    , style: createStyles {textIndent: "3em"}
    , paragraph: true
    }
    [ R.text "Every chef has a "
    , R.em [] [R.text "bi-weekly"]
    , R.text " schedule, and every order must be filed at least two weeks in advance — each menu has its own shipping date, and each chef has their own planned schedule to fill their orders. This allows chefs to "
    , R.em [] [R.text "care"]
    , R.text " about each meal and give their full attention to their craft, without having to worry about wasteful time constraints. Each chef:"
    ]
  , list {dense: true}
    [ listItem {}
      [ listItemIcon {} restaurantMenuIcon
      , listItemText
        {primary: "Manages their own work schedule"}
      ]
    , listItem {}
      [ listItemIcon {} restaurantMenuIcon
      , listItemText
        {primary: "Creates their own menus and portfolio"}
      ]
    , listItem {}
      [ listItemIcon {} restaurantMenuIcon
      , listItemText
        {primary: "Builds their own consumer base and market"}
      ]
    ]
  ]
