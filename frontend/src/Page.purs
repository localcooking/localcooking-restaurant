module Page where

import Links (SiteLinks (..))

import Prelude
import Data.Generic (class Generic, gEq)
import Control.Monad.Aff (Aff)



data Page
  = AboutPage
  | MenuPage

derive instance genericPage :: Generic Page

instance eqPage :: Eq Page where
  eq = gEq



makePage :: forall eff
          . SiteLinks
         -> { immediate :: Page
            , loadDetails :: Aff eff Page -- FIXME include context for websocket and UI queueing
            }
makePage x =
  { immediate: case x of
      RootLink -> MenuPage
      AboutLink -> AboutPage
  , loadDetails: case x of
      RootLink -> pure MenuPage
      AboutLink -> pure AboutPage
  }


initPage :: Page
initPage = MenuPage
