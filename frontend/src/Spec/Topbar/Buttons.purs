module Spec.Topbar.Buttons where

import Links (SiteLinks)
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)

import Prelude
import Data.URI.URI as URI
import Data.URI.Location (toLocation)
import Data.UUID (GENUUID)
import Data.Lens (Lens', lens)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.DOM.Props.PreventDefault (preventDefault)

import MaterialUI.Button (button)
import MaterialUI.Button as Button



type State =
  { localCooking :: LocalCookingState SiteLinks UserDetails
  }

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingAction (LocalCookingAction SiteLinks UserDetails)
  | Clicked SiteLinks

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)

getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> Array R.ReactElement
     -> T.Spec (Effects eff) State Unit Action
spec params@{siteLinks,toURI} prefix = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state
      Clicked x -> liftEff (siteLinks x)

    render :: T.Render State Unit Action
    render dispatch props state children =
      prefix <>
      []
      -- [ button
      --   { color: Button.primary
      --   , disabled: state.currentPage == MealsLink
      --   , onClick: mkEffFn1 preventDefault
      --   , onTouchTap: mkEffFn1 \e -> do
      --       preventDefault e
      --       dispatch (Clicked MealsLink)
      --   , href: URI.print $ toURI $ toLocation MealsLink
      --   , variant: Button.raised
      --   } [R.text "Meals"]
      -- , button
      --   { color: Button.secondary
      --   , disabled: state.currentPage == ChefsLink
      --   , onClick: mkEffFn1 preventDefault
      --   , onTouchTap: mkEffFn1 \e -> do
      --       preventDefault e
      --       dispatch (Clicked ChefsLink)
      --   , href: URI.print $ toURI $ toLocation ChefsLink
      --   , variant: Button.raised
      --   } [R.text "Chefs"]
      -- ]


topbarButtons :: forall eff
               . LocalCookingParams SiteLinks UserDetails (Effects eff)
              -> Array R.ReactElement
              -> R.ReactElement
topbarButtons params prefix =
  let {spec:reactSpec,dispatcher} =
        T.createReactSpec
          ( spec params prefix
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
        whileMountedLocalCooking
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
