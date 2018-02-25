module Spec.Content where

import Spec.Content.About (about)
import Spec.Content.Menu (menu)
import Links (SiteLinks (..))
import Page (Page (..), initPage)

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Markdown (markdown)
import React.Signal.WhileMounted as Signal
import Data.UUID (GENUUID)
import Control.Monad.Eff.Uncurried (mkEffFn2)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import MaterialUI.Types (createStyles)
import MaterialUI.Paper (paper)
import MaterialUI.Divider (divider)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Tabs (tabs, tab)
import MaterialUI.Tabs as Tabs

import IxSignal.Internal (IxSignal)



type State =
  { page :: Page
  }

initialState :: State
initialState =
  { page: initPage
  }

data Action
  = ChangedCurrentPage Page


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


spec :: forall eff. T.Spec eff State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedCurrentPage p -> void $ T.cotransform _ { page = p }

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
          [ paper {style: createStyles {minHeight: "30em"}}
            [ case state.page of
                AboutPage -> about
                MenuPage -> menu
            ]
          , typography
            { variant: Typography.caption
            , style: createStyles {marginTop: "5em", textAlign: "center"}
            }
            [ R.text "Copyright © Local Cooking Inc. 2018, All rights reserved" ]
          ]
        ]
      ]



content :: forall eff
         . { currentPageSignal :: IxSignal (Effects eff) Page
           } -> R.ReactElement
content {currentPageSignal} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
      reactSpec' = Signal.whileMountedIxUUID
                     currentPageSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x)) reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
