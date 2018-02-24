module Thermite.Window where

import Prelude
import Window (WindowSize (..))

import Data.Maybe (Maybe (..))
import Data.Lens (Lens', Prism', lens, prism', over)
import Data.UUID (GENUUID)
import Control.Coroutine (transform, transformCoTransformL, transformCoTransformR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Rec.Class (forever)

import React (ReactSpec, ReactThis)
import React.Signal.WhileMounted (whileMountedIxUUID)
import Thermite as T
import IxSignal.Internal (IxSignal)



type State a =
  { windowSize :: WindowSize
  , state :: a
  }

initialState :: forall state. state -> State state
initialState state = {state, windowSize: Pager}

_state :: forall state. Lens' (State state) state
_state = lens (_.state) (_ { state = _ })



data Action a
  = ChangedWindowSize WindowSize
  | Action a

_Action :: forall action. Prism' (Action action) action
_Action = prism' Action $ case _ of
  Action x -> Just x
  _ -> Nothing

performAction :: forall props action state eff
               . T.PerformAction eff state props action
              -> T.PerformAction eff (State state) props (Action action)
performAction performActionChild action props state = case action of
  ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
  Action a ->
    forever (transform (map (_.state)))
    `transformCoTransformL` performActionChild a props state.state
    `transformCoTransformR` forever (transform (over _state))

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


listening :: forall props action state eff render
           . IxSignal (Effects eff) WindowSize
          -> { spec :: ReactSpec props (State state) render (Effects eff)
             , dispatcher :: ReactThis props (State state) -> Action action -> T.EventHandler
             }
          -> ReactSpec props (State state) render (Effects eff)
listening windowSizeSignal {spec,dispatcher} =
  whileMountedIxUUID windowSizeSignal (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x)) spec
