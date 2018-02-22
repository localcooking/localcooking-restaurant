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
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Console (CONSOLE, log, error)

import Signal.Internal (subscribe)
import Signal.Internal as Signal
import Signal.Time (debounce)
import Signal.DOM (windowDimensions)

import Thermite as T
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT, injectTapEvent)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location)
import DOM.HTML.Location (hostname, protocol, port)


main :: Eff ( console        :: CONSOLE
            , injectTapEvent :: INJECT_TAP_EVENT
            , ref            :: REF
            , dom            :: DOM
            , timer          :: TIMER
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
    out <- Signal.make initWidth
    flip subscribe sig \w' -> do
      lastWindowWidth <- readRef windowWidthRef
      when (w'.w /= lastWindowWidth) $ do
        writeRef windowWidthRef w'.w
        Signal.set w'.w out
    pure out

  flip subscribe windowSizeSignal \windowWidth -> do

    let windowSize = widthToWindowSize windowWidth

    T.defaultMain
      ( spec
          { toURI : \location -> toURI {scheme, authority, location}
          }
      )
      initialState unit
