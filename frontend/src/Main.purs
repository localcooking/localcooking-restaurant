module Main where

import Spec (app)
import Window (widthToWindowSize)
import Links (SiteLinks (..), ThirdPartyLoginReturnLinks (..),siteLinksParser, siteLinksToDocumentTitle, toLocation, thirdPartyLoginReturnLinksParser)
import Types.Env (env)
import Login.Error (AuthError, PreliminaryAuthToken (..))
import Login.Storage (getStoredAuthToken)
import LocalCooking.Common.AuthToken (AuthToken)
import Client.Dependencies.AuthToken (AuthTokenSparrowClientQueues)

import Sparrow.Client.Queue (newSparrowClientQueues, sparrowClientQueues)
import Sparrow.Client (unpackClient, allocateDependencies)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.Time.Duration (Milliseconds (..))
import Data.URI (Scheme (..), Host (..), Port (..), Authority (..))
import Data.URI.URI as URI
import Data.URI.Location (toURI, fromURI, Location (..))
import Data.URI.Query (Query (..))
import Data.Int.Parse (parseInt, toRadix)
import Data.String (takeWhile) as String
import Data.UUID (GENUUID)
import Data.Traversable (traverse_)
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Argonaut (jsonParser, encodeJson, decodeJson)
import Data.StrMap as StrMap
import Text.Parsing.StringParser (runParser)
import Control.Monad.Aff (runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION, throw, throwException)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM, registerShim)

import Queue.One as One
import Signal.Internal as Signal
import IxSignal.Internal as IxSignal
import Signal.Time (debounce)
import Signal.DOM (windowDimensions)

import React as R
import ReactDOM (render)
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT, injectTapEvent)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location, document, history)
import DOM.HTML.Window.Extra (onPopState, queryParams, pushState', replaceState')
import DOM.HTML.Document (body)
import DOM.HTML.Location (hostname, protocol, port, pathname, hash, href)
import DOM.HTML.Types (HISTORY, htmlElementToElement)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)


-- | All top-level effects
type Effects =
  ( console            :: CONSOLE
  , injectTapEvent     :: INJECT_TAP_EVENT
  , set_immediate_shim :: SET_IMMEDIATE_SHIM
  , ref                :: REF
  , dom                :: DOM
  , timer              :: TIMER
  , uuid               :: GENUUID
  , exception          :: EXCEPTION
  , history            :: HISTORY
  , now                :: NOW
  , ws                 :: WEBSOCKET
  , ajax               :: AJAX
  , webStorage         :: WEB_STORAGE
  , scrypt             :: SCRYPT
  )


main :: Eff Effects Unit
main = do
  log "Starting Local Cooking frontend..."

  injectTapEvent
  _ <- registerShim


  w <- window
  l <- location w
  h <- history w
  scheme <- Just <<< Scheme <<< String.takeWhile (\c -> c /= ':') <$> protocol l
  authority <- do
    host <- hostname l
    p' <- port l
    p <- case parseInt p' (toRadix 10) of
      Nothing ->  pure Nothing -- undefined <$ error "Somehow couldn't parse port"
      Just x -> pure (Just (Port x))
    pure $ Authority Nothing [Tuple (NameAddress host) p]

  currentPageSignal <- do
    initSiteLink <- do
      -- remove auth token
      -- case StrMap.lookup "authToken" (queryParams l) of
      --   Nothing -> pure unit
      --   Just _ -> removeQueryParam l "authToken" -- FIXME causes a page refresh

      -- -- remove residual facebook gunk from fragment
      -- h' <- hash l
      -- when (h' /= "") $ setHash "" l

      -- parse foo.com/pathname
      p <- href l
      case URI.parse p of
        Left e -> throw $ "Href parsing error: " <> show e
        Right uri -> case fromURI uri of
          Nothing -> throw $ "URI can't be a location: " <> show uri
          Just {location: location@(Location _ mQuery _)} -> case siteLinksParser location of
            Nothing -> do
              -- FIXME should be able to handle bogus URIs
              throw $ "Location can't be a SiteLinks: " <> show location
            Just x -> do
              -- adjust for possibly present authToken
              case mQuery of
                Nothing -> pure unit
                Just (Query qs) -> case StrMap.lookup "authToken" (StrMap.fromFoldable qs) of
                  Nothing -> pure unit
                  Just _ -> replaceState' x

              pure x

    -- fetch resources - FIXME use sparrow to drive it - via currentPageSignal?
    sig <- IxSignal.make initSiteLink
    onPopState
      (\siteLink -> IxSignal.set siteLink sig
      ) w
    pure sig


  -- history driver - write to this to change the page, with history.
  -- FIXME hard, firm, soft history links
  siteLinksSignal <- do
    q <- One.newQueue
    One.onQueue q \(x :: SiteLinks) -> do
      pushState' x
      IxSignal.set x currentPageSignal
    pure (One.writeOnly q)


  windowSizeSignal <- do
    -- debounces and only relays when the window size changes
    sig <- debounce (Milliseconds 100.0) =<< windowDimensions
    initWidth <- (\w' -> w'.w) <$> Signal.get sig
    windowWidthRef <- newRef initWidth
    let initWindowSize = widthToWindowSize initWidth
    out <- IxSignal.make initWindowSize
    flip Signal.subscribe sig \w' -> do
      lastWindowWidth <- readRef windowWidthRef
      when (w'.w /= lastWindowWidth) $ do
        writeRef windowWidthRef w'.w
        let size = widthToWindowSize w'.w
        IxSignal.set size out
    pure out



  ( authTokenQueues :: AuthTokenSparrowClientQueues Effects
    ) <- newSparrowClientQueues

  -- Sparrow dependencies
  allocateDependencies (scheme == Just (Scheme "https")) authority $ do
    unpackClient (Topic ["authToken"]) (sparrowClientQueues authTokenQueues)


  ( preliminaryAuthToken :: PreliminaryAuthToken
    ) <- map PreliminaryAuthToken $ case env.authToken of
      PreliminaryAuthToken Nothing -> map Right <$> getStoredAuthToken
      PreliminaryAuthToken (Just eErrX) -> pure (Just eErrX)


  -- React.js view
  let props = unit
      {spec: reactSpec, dispatcher} =
        app
          { toURI : \location -> toURI {scheme, authority: Just authority, location}
          , windowSizeSignal
          , currentPageSignal
          , siteLinks: One.putQueue siteLinksSignal
          , development: env.development
          , preliminaryAuthToken
          , authTokenQueues
          }
      component = R.createClass reactSpec
  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body =<< document w
