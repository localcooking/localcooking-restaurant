module Spec.Content.UserDetails where

import Links (SiteLinks (UserDetailsLink), UserDetailsLinks (..))
import Spec.Content.UserDetails.General (general)
import Spec.Content.UserDetails.Security (security)

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer
import MaterialUI.Divider (divider)
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)

import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Partial.Unsafe (unsafePartial)


type State =
  { page :: SiteLinks
  }

initialState :: {initSiteLinks :: SiteLinks} -> State
initialState {initSiteLinks} =
  { page: initSiteLinks
  }

data Action
  = ChangedCurrentPage SiteLinks

type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  | eff)


spec :: forall eff
      . { siteLinks :: SiteLinks -> Eff (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec {siteLinks} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedCurrentPage x -> void $ T.cotransform _ { page = x }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ unsafePartial
      $ case state.page of -- TODO pack currentPageSignal listener to this level, so
                            -- side buttons aren't redrawn
          UserDetailsLink mUserDetails -> case mUserDetails of
            Nothing -> general
            Just x -> case x of
              UserDetailsGeneralLink -> general
      ]


userDetails :: forall eff
             . { currentPageSignal :: IxSignal (Effects eff) SiteLinks
               , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
               }
            -> R.ReactElement
userDetails {currentPageSignal,siteLinks} =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        }
      {spec: reactSpec, dispatcher} = T.createReactSpec (spec {siteLinks}) (initialState init)
      reactSpec' =
        Signal.whileMountedIxUUID
          currentPageSignal
          (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
        reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
