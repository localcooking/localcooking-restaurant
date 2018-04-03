module Spec.Content where

import Spec.Content.Root (root)
import Spec.Content.Chefs (chefs)
import Spec.Content.Meals (meals)
import Spec.Content.Register (register)
import Spec.Flags.USA (usaFlag, usaFlagViewBox)
import Spec.Flags.Colorado (coloradoFlag, coloradoFlagViewBox)
import Links (SiteLinks (..))
import Client.Dependencies.Register (RegisterSparrowClientQueues)
import Window (WindowSize (Laptop))

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import MaterialUI.Types (createStyles)
import MaterialUI.Paper (paper)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.SvgIcon (svgIcon)
import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



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
      . { registerQueues    :: RegisterSparrowClientQueues (Effects eff)
        , windowSizeSignal  :: IxSignal (Effects eff) WindowSize
        , siteLinks         :: SiteLinks -> Eff (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec {registerQueues,windowSizeSignal,siteLinks} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedCurrentPage p ->
        void $ T.cotransform _ { page = p }
      ChangedWindowSize w ->
        void $ T.cotransform _ { windowSize = w }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ R.main [RP.style {marginTop: "4.5em"}]
        [ paper
          { style: if state.windowSize < Laptop
                      then createStyles
                              { width: "100%"
                              , position: "relative"
                              }
                      else createStyles
                              { maxWidth: "80em"
                              , width: "100%"
                              , marginLeft: "auto"
                              , marginRight: "auto"
                              , padding: "1em"
                              , position: "relative"
                              }
          }
          [ R.div [RP.style {minHeight: "30em", padding: "1em"}]
            [ case state.page of
                RootLink -> root {windowSizeSignal}
                ChefsLink -> chefs
                MealsLink -> meals
                RegisterLink ->
                  register
                    { registerQueues
                    , toRoot: siteLinks RootLink
                    }
            ]
          ]
        ]
      , typography
        { variant: Typography.caption
        , style: createStyles {marginTop: "5em"}
        , align: Typography.center
        }
        [ R.text "Copyright © Local Cooking Inc. 2018, All rights reserved." ]
      , typography
        { variant: Typography.caption
        , align: Typography.center
        }
        [ R.text "Proudly made in Golden, Colorado, The United States of America."
        ]
      , svgIcon {viewBox: coloradoFlagViewBox} coloradoFlag
      , svgIcon {viewBox: usaFlagViewBox} usaFlag
      ]



content :: forall eff
         . { currentPageSignal :: IxSignal (Effects eff) SiteLinks
           , windowSizeSignal  :: IxSignal (Effects eff) WindowSize
           , registerQueues    :: RegisterSparrowClientQueues (Effects eff)
           , siteLinks         :: SiteLinks -> Eff (Effects eff) Unit
           } -> R.ReactElement
content {currentPageSignal,registerQueues,windowSizeSignal,siteLinks} =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec (spec {registerQueues,windowSizeSignal,siteLinks}) (initialState init)
      reactSpec' = Signal.whileMountedIxUUID
                     currentPageSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
                 $ Signal.whileMountedIxUUID
                     windowSizeSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
                   reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
