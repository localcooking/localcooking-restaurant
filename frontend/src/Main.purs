module Main where

import Spec (app)
import Spec.Snackbar (SnackbarMessage (..), RedirectError (..))
import Window (widthToWindowSize)
import Links (SiteLinks (..), initSiteLinks, onPopState, pushState', replaceState')
import Types.Env (env)
import Login.Error (PreliminaryAuthToken (..))
import Login.Storage (getStoredAuthToken)
import Client.Dependencies.AuthToken (AuthTokenSparrowClientQueues)
import Client.Dependencies.Register (RegisterSparrowClientQueues)
import Client.Dependencies.UserDetails.Email (UserDetailsEmailSparrowClientQueues)
import LocalCooking.Common.AuthToken (AuthToken)

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
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM, registerShim)

import Queue (WRITE, READ)
import Queue.One as One
import Signal.Internal as Signal
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Signal.Time (debounce)
import Signal.DOM (windowDimensions)

import React as R
import ReactDOM (render)
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT, injectTapEvent)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location, document, history)
import DOM.HTML.Document (body)
import DOM.HTML.Location (hostname, protocol, port)
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


  ( preliminaryAuthToken :: PreliminaryAuthToken
    ) <- map PreliminaryAuthToken $ case env.authToken of
      PreliminaryAuthToken Nothing -> map Right <$> getStoredAuthToken
      PreliminaryAuthToken (Just eErrX) -> pure (Just eErrX)

  ( errorMessageQueue :: One.Queue (read :: READ, write :: WRITE) Effects SnackbarMessage
    ) <- One.newQueue

  ( authTokenSignal :: IxSignal Effects (Maybe AuthToken)
    ) <- IxSignal.make Nothing

  -- for `back` compatibility while being driven by `siteLinksSignal`
  ( currentPageSignal :: IxSignal Effects SiteLinks
    ) <- do
    initSiteLink <- do
      -- initial redirects
      x <- initSiteLinks
      case preliminaryAuthToken of
        PreliminaryAuthToken (Just (Right _)) -> case x of
          RegisterLink -> do
            One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
            replaceState' RootLink h
            pure RootLink
          _ -> pure x
        _ -> case x of
          UserDetailsLink -> do
            One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
            replaceState' RootLink h
            pure RootLink
          _ -> pure x

    -- fetch resources - FIXME use sparrow to drive it - via currentPageSignal?
    sig <- IxSignal.make initSiteLink
    flip onPopState w \siteLink -> do
      let continue x = IxSignal.set x sig
      -- Top level soft redirections - no history change, adjust for browser back-button:
      case siteLink of
        RegisterLink -> do
          mAuth <- IxSignal.get authTokenSignal
          case mAuth of
            Nothing -> continue siteLink
            Just _ -> do
              One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
              replaceState' RootLink h
              continue RootLink
        UserDetailsLink -> do
          mAuth <- IxSignal.get authTokenSignal
          case mAuth of
            Just _ -> continue siteLink
            Nothing -> do
              One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
              replaceState' RootLink h
              continue RootLink
        _ -> continue siteLink

    pure sig


  -- history driver - write to this to change the page, with history.
  ( siteLinksSignal :: One.Queue (write :: WRITE) Effects SiteLinks
    ) <- do
    q <- One.newQueue
    One.onQueue q \(siteLink :: SiteLinks) -> do
      -- only respect changed pages
      y <- IxSignal.get currentPageSignal
      when (y /= siteLink) $ do
        let continue x = do
              pushState' x h
              IxSignal.set x currentPageSignal
        -- redirect rules
        case siteLink of
          RegisterLink -> do
            mAuth <- IxSignal.get authTokenSignal
            case mAuth of
              Nothing -> continue siteLink
              Just _ -> do
                One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
                continue RootLink
          UserDetailsLink -> do
            mAuth <- IxSignal.get authTokenSignal
            case mAuth of
              Just _ -> continue siteLink
              Nothing -> do
                One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
                continue RootLink
          _ -> continue siteLink
    pure (One.writeOnly q)


  -- rediect for async logouts
  let gotAuth mAuth = do
        siteLink <- IxSignal.get currentPageSignal
        let continue = One.putQueue siteLinksSignal
        case mAuth of
          Nothing -> case siteLink of
            UserDetailsLink -> do
              One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectUserDetailsNoAuth)
              continue RootLink
            _ -> pure unit
          Just _ -> case siteLink of
            RegisterLink -> do
              One.putQueue errorMessageQueue (SnackbarMessageRedirect RedirectRegisterAuth)
              continue RootLink
            _ -> pure unit
  IxSignal.subscribe gotAuth authTokenSignal


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



  -- Sparrow dependencies
  ( authTokenQueues :: AuthTokenSparrowClientQueues Effects
    ) <- newSparrowClientQueues
  ( registerQueues :: RegisterSparrowClientQueues Effects
    ) <- newSparrowClientQueues
  ( userDetailsEmailQueues :: UserDetailsEmailSparrowClientQueues Effects
    ) <- newSparrowClientQueues
  allocateDependencies (scheme == Just (Scheme "https")) authority $ do
    unpackClient (Topic ["authToken"]) (sparrowClientQueues authTokenQueues)
    unpackClient (Topic ["register"]) (sparrowClientQueues registerQueues)
    unpackClient (Topic ["userDetails","email"]) (sparrowClientQueues userDetailsEmailQueues)



  -- Run User Interface
  let props = unit
      {spec: reactSpec, dispatcher} =
        app
          { toURI : \location -> toURI {scheme, authority: Just authority, location}
          , windowSizeSignal
          , currentPageSignal
          , siteLinks: One.putQueue siteLinksSignal
          , development: env.development
          , preliminaryAuthToken
          , errorMessageQueue
          , authTokenSignal
          , authTokenQueues
          , registerQueues
          , userDetailsQueues:
            { emailQueues: userDetailsEmailQueues
            }
          }
      component = R.createClass reactSpec
  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body =<< document w
