module Spec.Snackbar where

import Client.Dependencies.AuthToken (AuthTokenFailure (..))
import Login.Error (AuthError (..))

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Markdown (markdown)
import React.Signal.WhileMounted as Signal
import React.Queue.WhileMounted as Queue
import Data.UUID (GENUUID)
import Data.Nullable (toNullable)
import Data.Time.Duration (Milliseconds (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Control.Monad.Eff.Uncurried (mkEffFn2)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import MaterialUI.Types (createStyles)
import MaterialUI.Paper (paper)
import MaterialUI.Divider (divider)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Tabs (tabs, tab)
import MaterialUI.Tabs as Tabs
import MaterialUI.Snackbar (snackbar)

import Queue.One as One



type State =
  { authFailure :: Maybe AuthTokenFailure
  , authError :: Maybe AuthError
  }

initialState :: State
initialState =
  { authFailure: Nothing
  , authError: Nothing
  }

data Action
  = GotAuthFailure AuthTokenFailure
  | ClearAuthFailure
  | GotAuthError AuthError
  | ClearAuthError

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff. T.Spec (Effects eff) State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotAuthFailure x -> void $ T.cotransform _ { authFailure = Just x }
      ClearAuthFailure -> void $ T.cotransform _ { authFailure = Nothing }
      GotAuthError x -> void $ T.cotransform _ { authError = Just x }
      ClearAuthError -> void $ T.cotransform _ { authError = Nothing }

    render :: T.Render State _ Action
    render dispatch props state children =
      [ snackbar
        { open: case state.authFailure of
            Nothing -> case state.authError of
              Nothing -> false
              Just _ -> true
            Just _ -> true
        , autoHideDuration: toNullable $ Just $ Milliseconds 10000.0
        -- , resumeHideDuration: toNullable $ Just $ Milliseconds 0.0
        , onClose: mkEffFn2 \_ _ -> do
            dispatch ClearAuthError
            dispatch ClearAuthFailure
        , message: R.div []
          [ case state.authFailure of
              Nothing -> R.text ""
              Just x -> case x of
                BadPassword -> R.text "Password incorrect, please try again."
                EmailDoesntExist -> R.text "Email address not found, please register."
          , case state.authError of
              Nothing -> R.text ""
              Just x -> case x of
                FBLoginReturnBad code msg -> R.text $ "Bad Facebook login response: " <> code <> ", " <> msg
                FBLoginReturnDenied desc -> R.text $ "Facebook login denied: " <> desc
                FBLoginReturnBadParse -> R.text "Internal error: Facebook login return unparsable."
                FBLoginReturnNoUser -> R.text "Facebook user not recognized, please link your account."
                AuthExistsFailure -> R.text "Warning: You've been logged out; your session expired."
          ]
        }
      ]



messages :: forall eff
          . { authErrorSignal :: One.Queue (read :: One.READ) (Effects eff) (Either AuthError AuthTokenFailure)
            } -> R.ReactElement
messages {authErrorSignal} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
      reactSpec' =
        Queue.whileMountedOne
          authErrorSignal
          (\this eX -> unsafeCoerceEff $ dispatcher this $ case eX of
              Left err -> GotAuthError err
              Right fail -> GotAuthFailure fail)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
