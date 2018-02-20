module Main where

import Spec (spec, initialState)
import Window (widthToWindowSize)

import Prelude
import Data.Time.Duration (Milliseconds (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Console (CONSOLE, log)

import Signal.Internal (subscribe)
import Signal.Internal as Signal
import Signal.Time (debounce)
import Signal.DOM (windowDimensions)

import Thermite as T
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT, injectTapEvent)
import DOM (DOM)


main :: Eff ( console        :: CONSOLE
            , injectTapEvent :: INJECT_TAP_EVENT
            , ref            :: REF
            , dom            :: DOM
            , timer          :: TIMER
            ) Unit
main = do
  log "Starting Local Cooking"

  injectTapEvent

  windowSizeSignal <- do
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

    T.defaultMain spec initialState unit
