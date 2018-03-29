module Main where

import Spec (app)
import Window (widthToWindowSize)
import Links (SiteLinks (..), ThirdPartyLoginReturnLinks (..),siteLinksParser, siteLinksToDocumentTitle, toLocation, thirdPartyLoginReturnLinksParser)
import Page (makePage)
import Types.Env (env)
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
import Data.URI.Location (toURI)
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
import DOM.HTML.Window.Extra (onPopState, queryParams, removeQueryParam)
import DOM.HTML.Document (body)
import DOM.HTML.History (pushState, URL (..), DocumentTitle (..))
import DOM.HTML.Location (hostname, protocol, port, pathname)
import DOM.HTML.Types (HISTORY, htmlElementToElement)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)


main :: Eff ( console        :: CONSOLE
            , injectTapEvent :: INJECT_TAP_EVENT
            , ref            :: REF
            , dom            :: DOM
            , timer          :: TIMER
            , uuid           :: GENUUID
            , exception      :: EXCEPTION
            , history        :: HISTORY
            , now            :: NOW
            , ws             :: WEBSOCKET
            , ajax           :: AJAX
            ) Unit
main = do
  log "Starting Local Cooking frontend..."

  injectTapEvent


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

  -- FIXME rip out - no need for history-enriched redirection.
  toSiteLinksSignal <- One.newQueue -- Just a hack for redirections, because Eff isn't MonadRec via `siteLinksSignal`

  currentPageSignal <- do
    initSiteLink <- do
      case StrMap.lookup "authToken" (queryParams l) of
        Nothing -> pure unit
        Just _ -> removeQueryParam l "authToken"
      p <- pathname l
      if p == ""
        then pure RootLink
        else case runParser siteLinksParser p of
          Left e1 -> case runParser thirdPartyLoginReturnLinksParser p of
            Left e2 -> throw $ "Parsing errors: " <> show e1
                            <> ", " <> show e2
            Right x -> case x of
              -- FIXME rip out, won't need
              FacebookLoginReturn ->
                case StrMap.lookup "state" (queryParams l) of
                  Nothing -> throw $ "No `state` key in query params: " <> show (queryParams l)
                  Just s -> case jsonParser s >>= decodeJson of
                    Left e -> throw $ "URI Query parsing error: " <> e
                    Right (x' :: Maybe Unit) -> do -- FIXME Facebook state
                      -- redirect
                      One.putQueue toSiteLinksSignal RootLink
                      -- assumed original
                      pure RootLink
          Right x -> pure x
    let {immediate,loadDetails} = makePage initSiteLink
    sig <- IxSignal.make immediate
    flip runAff_ loadDetails \eX -> case eX of
      Left e -> throwException e
      Right x -> IxSignal.set x sig
    onPopState
      (\x -> do
        {immediate,loadDetails} <- case decodeJson (unsafeFromForeign x) of
          Left e -> throw e
          Right (x :: SiteLinks) -> pure (makePage x)
        IxSignal.set immediate sig
        flip runAff_ loadDetails \eX -> case eX of -- FIXME resource loading section
          Left e -> throwException e
          Right x' -> IxSignal.set x' sig
      ) w
    pure sig

  siteLinksSignal <- do
    q <- One.newQueue
    One.onQueue q \(x :: SiteLinks) -> do
      pushState (toForeign (encodeJson x)) (siteLinksToDocumentTitle x) (URL (show x)) h
      let {immediate,loadDetails} = makePage x
      IxSignal.set immediate currentPageSignal
      flip runAff_ loadDetails \eX -> case eX of
        Left e -> throwException e
        Right x -> IxSignal.set x currentPageSignal
    pure (One.writeOnly q)

  One.onQueue toSiteLinksSignal (One.putQueue siteLinksSignal)
  -- end hack

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

  toLocalCooking <- One.newQueue


  ( authTokenQueues :: AuthTokenSparrowClientQueues _
    ) <- newSparrowClientQueues

  -- Sparrow dependencies
  allocateDependencies (scheme == Just (Scheme "https")) authority $ do
    unpackClient (Topic ["authToken"]) (sparrowClientQueues authTokenQueues)


  -- React.js view
  let props = unit
      {spec: reactSpec, dispatcher} =
        app
          { toURI : \location -> toURI {scheme, authority: Just authority, location}
          , windowSizeSignal
          , currentPageSignal
          , siteLinks: One.putQueue siteLinksSignal
          , development: env.development
          }
      component = R.createClass reactSpec
  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body =<< document w
