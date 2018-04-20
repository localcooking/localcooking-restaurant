module Spec.Content where

import Spec.Content.Root (root)
import Spec.Content.UserDetails (userDetails)
import Links (SiteLinks (..))
import LocalCooking.Window (WindowSize)

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.Location (Location)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Partial.Unsafe (unsafePartial)



type State =
  { page :: SiteLinks
  , windowSize :: WindowSize
  }

initialState :: {initSiteLinks :: SiteLinks, initWindowSize :: WindowSize} -> State
initialState {initSiteLinks,initWindowSize} =
  { page: initSiteLinks
  , windowSize: initWindowSize
  }

data Action
  = ChangedCurrentPage SiteLinks
  | ChangedWindowSize WindowSize


type Effects eff =
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , console   :: CONSOLE
  , scrypt    :: SCRYPT
  | eff)


spec :: forall eff
      . { windowSizeSignal  :: IxSignal (Effects eff) WindowSize
        , siteLinks         :: SiteLinks -> Eff (Effects eff) Unit
        , currentPageSignal :: IxSignal (Effects eff) SiteLinks
        , toURI             :: Location -> URI
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  { windowSizeSignal
  , currentPageSignal
  , siteLinks
  , toURI
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedCurrentPage p ->
        void $ T.cotransform _ { page = p }
      ChangedWindowSize w ->
        void $ T.cotransform _ { windowSize = w }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ case state.page of
          RootLink ->
            root
              { windowSizeSignal
              , toURI
              }
          _ -> R.text ""
      ]



content :: forall eff
         . { currentPageSignal :: IxSignal (Effects eff) SiteLinks
           , windowSizeSignal  :: IxSignal (Effects eff) WindowSize
           , siteLinks         :: SiteLinks -> Eff (Effects eff) Unit
           , toURI             :: Location -> URI
           } -> R.ReactElement
content
  { currentPageSignal
  , windowSizeSignal
  , siteLinks
  , toURI
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { windowSizeSignal
            , currentPageSignal
            , siteLinks
            , toURI
            }
          )
          (initialState init)
      reactSpec' = Signal.whileMountedIxUUID
                     currentPageSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
                 $ Signal.whileMountedIxUUID
                     windowSizeSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
                   reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
