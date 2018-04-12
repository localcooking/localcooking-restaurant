module Spec.Topbar.Buttons where

import Links (SiteLinks)
import LocalCooking.Links.Class (toLocation)

import Prelude
import Data.URI (URI)
import Data.URI.URI as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.SVG as RS
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import React.Signal.WhileMounted as Signal

import MaterialUI.Button (button)
import MaterialUI.Button as Button

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


type State =
  { currentPage :: SiteLinks
  }

initialState :: {initSiteLinks :: SiteLinks} -> State
initialState {initSiteLinks} =
  { currentPage: initSiteLinks
  }

data Action
  = ChangedCurrentPage SiteLinks
  | Clicked SiteLinks


spec :: forall eff
      . { siteLinks :: SiteLinks -> Eff (Effects eff) Unit
        , toURI :: Location -> URI
        }
     -> T.Spec (Effects eff) State Unit Action
spec {siteLinks,toURI} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedCurrentPage p -> void $ T.cotransform _ { currentPage = p }
      Clicked x -> liftEff (siteLinks x)

    render :: T.Render State Unit Action
    render dispatch props state children = []
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
               . { currentPageSignal :: IxSignal (Effects eff) SiteLinks
                 , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
                 , toURI :: Location -> URI
                 } -> R.ReactElement
topbarButtons
  { toURI
  , siteLinks
  , currentPageSignal
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        }
      {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          { toURI
          , siteLinks
          }
        )
        (initialState init)
      reactSpec' =
          Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
