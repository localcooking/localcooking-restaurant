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
import Data.UUID (GENUUID)
import Data.Nullable (toNullable)
import Data.Time.Duration (Milliseconds (..))
import Data.Maybe (Maybe (..))
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


type State = Unit

initialState :: State
initialState = unit

type Action = Unit


spec :: { authFailure :: Maybe AuthTokenFailure
        , authError :: Maybe AuthError
        }
     -> T.Spec _ State _ Action
spec {authFailure,authError} = T.simpleSpec performAction render
  where
    performAction action props state = pure unit

    render :: T.Render State _ Action
    render dispatch props state children =
      [ snackbar
        { open: case authFailure of
            Nothing -> case authError of
              Nothing -> false
              Just _ -> true
            Just _ -> true
        , autoHideDuration: toNullable $ Just $ Milliseconds 10000.0
        , message: R.div []
          [ case authFailure of
              Nothing -> R.text ""
              Just x -> case x of
                BadPassword -> R.text "Password incorrect, please try again."
                EmailDoesntExist -> R.text "Email address not found, please register."
          , case authError of
              Nothing -> R.text ""
              Just x -> case x of
                FBLoginReturnBad code msg -> R.text $ "Bad Facebook login respose: " <> code <> ", " <> msg
                FBLoginReturnDenied desc -> R.text $ "Facebook login denied: " <> desc
                FBLoginReturnBadParse -> R.text "Internal error: Facebook login return unparsable."
                FBLoginReturnNoUser -> R.text "Facebook user not recognized, please link your account."
                AuthExistsFailure -> R.text "Warning: You've been logged out; your session expired."
          ]
        }
      ]



messages :: forall eff
          . { authFailure :: Maybe AuthTokenFailure
            , authError :: Maybe AuthError
            } -> R.ReactElement
messages params =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec params) initialState
  in  R.createElement (R.createClass reactSpec) unit []
