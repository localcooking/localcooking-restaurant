module Page where

import Links (SiteLinks (..))

import Prelude
import Data.Generic (class Generic, gEq)
import Control.Monad.Aff (Aff)



data Page
  = AboutPage
  | RootPage
  | MealsPage
  | ChefsPage

derive instance genericPage :: Generic Page

instance eqPage :: Eq Page where
  eq = gEq



-- FIXME consider a resource oriented architecture
makePage :: forall eff
          . SiteLinks
         -> { immediate :: Page
            , loadDetails :: Aff eff Page -- FIXME include context for websocket and UI queueing
            }
makePage x =
  { immediate: case x of
      RootLink -> RootPage
      AboutLink -> AboutPage
      MealsLink -> MealsPage
      ChefsLink -> ChefsPage
  , loadDetails: case x of
      RootLink -> pure RootPage
      AboutLink -> pure AboutPage
      MealsLink -> pure MealsPage
      ChefsLink -> pure ChefsPage
  }


initPage :: Page
initPage = RootPage


pageBacklink :: Page -> SiteLinks
pageBacklink x = case x of
  RootPage  -> RootLink
  AboutPage -> AboutLink
  MealsPage -> MealsLink
  ChefsPage -> ChefsLink
