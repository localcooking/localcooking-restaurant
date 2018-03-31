module DOM.HTML.Window.Extra (onPopState, queryParams, pushState', replaceState') where

import Links (SiteLinks, toLocation, siteLinksToDocumentTitle)

import Prelude
import Data.Either (Either (..))
import Data.StrMap (StrMap)
import Data.URI.Location as Location
import Data.Foreign (Foreign, toForeign, unsafeFromForeign)
import Data.Argonaut (encodeJson, decodeJson)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window, Location, History, HISTORY)
import DOM.HTML.Window (history)
import DOM.HTML.History (pushState, replaceState, URL (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, mkEffFn1, runEffFn2)
import Control.Monad.Eff.Exception (EXCEPTION, throw)


foreign import onPopStateImpl :: forall eff. EffFn2 eff (EffFn1 eff Foreign Unit) Window Unit

onPopState :: forall eff
            . (SiteLinks -> Eff (dom :: DOM, exception :: EXCEPTION | eff) Unit)
           -> Window
           -> Eff (dom :: DOM, exception :: EXCEPTION | eff) Unit
onPopState go w =
  onPopState' \fgn -> case decodeJson (unsafeFromForeign fgn) of
    Left e -> throw e
    Right (x :: SiteLinks) -> go x
  where
    onPopState' f = runEffFn2 onPopStateImpl (mkEffFn1 f) w


foreign import queryParams :: Location -> StrMap String

-- foreign import removeQueryParamImpl :: forall eff. EffFn2 (dom :: DOM | eff) Location String Unit

-- removeQueryParam :: forall eff. Location -> String -> Eff (dom :: DOM | eff) Unit
-- removeQueryParam = runEffFn2 removeQueryParamImpl



pushState' :: forall eff. SiteLinks -> History -> Eff (history :: HISTORY | eff) Unit
pushState' x h = do
  pushState
    (toForeign $ encodeJson x)
    (siteLinksToDocumentTitle x)
    (URL $ Location.printLocation $ toLocation x)
    h


replaceState' :: forall eff. SiteLinks -> History -> Eff (history :: HISTORY | eff) Unit
replaceState' x h = do
  replaceState
    (toForeign $ encodeJson x)
    (siteLinksToDocumentTitle x)
    (URL $ Location.printLocation $ toLocation x)
    h
