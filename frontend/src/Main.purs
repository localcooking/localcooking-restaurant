module Main where

import Spec (spec, initialState)
import Window (widthToWindowSize)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Time.Duration (Milliseconds (..))
import Data.URI (Scheme (..), Host (..), Port (..), Authority (..))
import Data.URI.Location (toURI)
import Data.Int.Parse (parseInt, toRadix)
import Data.Typelevel.Undefined (undefined)
import Data.String (takeWhile) as String
import Data.UUID (GENUUID)
import Data.Traversable (traverse_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Console (CONSOLE, log, error)

import Signal.Internal as Signal
import IxSignal.Internal as IxSignal
import Signal.Time (debounce)
import Signal.DOM (windowDimensions)

import React as R
import ReactDOM (render)
import Thermite as T
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT, injectTapEvent)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location, document)
import DOM.HTML.Document (body)
import DOM.HTML.Location (hostname, protocol, port)
import DOM.HTML.Types (htmlElementToElement)


main :: Eff ( console        :: CONSOLE
            , injectTapEvent :: INJECT_TAP_EVENT
            , ref            :: REF
            , dom            :: DOM
            , timer          :: TIMER
            , uuid           :: GENUUID
            , exception      :: EXCEPTION
            ) Unit
main = do
  log "Starting Local Cooking"

  injectTapEvent


  w <- window
  l <- location w
  scheme <- Just <<< Scheme <<< String.takeWhile (\c -> c /= ':') <$> protocol l
  authority <- do
    host <- hostname l
    p' <- port l
    case parseInt p' (toRadix 10) of
      Nothing -> undefined <$ error "Somehow couldn't parse port"
      Just p -> pure $ Just $ Authority Nothing [Tuple (NameAddress host) (Just (Port p))]

  windowSizeSignal <- do
    -- debounces and only relays when the window size changes
    sig <- debounce (Milliseconds 100.0) =<< windowDimensions
    initWidth <- (\w' -> w'.w) <$> Signal.get sig
    windowWidthRef <- newRef initWidth
    out <- IxSignal.make (widthToWindowSize initWidth)
    flip Signal.subscribe sig \w' -> do
      lastWindowWidth <- readRef windowWidthRef
      when (w'.w /= lastWindowWidth) $ do
        writeRef windowWidthRef w'.w
        IxSignal.set (widthToWindowSize w'.w) out
    pure out



  let props = unit
      {spec: reactSpec, dispatcher} =
        let x = spec
                  { toURI : \location -> toURI {scheme, authority, location}
                  , windowSizeSignal
                  }
        in  T.createReactSpec x initialState
      component = R.createClass reactSpec
  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body =<< document w
