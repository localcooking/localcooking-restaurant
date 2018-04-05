module Spec.Snackbar where

import Client.Dependencies.AuthToken (AuthTokenFailure (..))
import Login.Error (AuthError (..))

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.Queue.WhileMounted as Queue
import Data.Nullable (toNullable)
import Data.Time.Duration (Milliseconds (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.List (List (..))
import Data.List as List
import Control.Monad.Base (liftBase)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Uncurried (mkEffFn2)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)

import MaterialUI.Snackbar (snackbar)

import Queue (READ)
import Queue.One as One



data UserDetailsError
  = UserDetailsEmailNoInitOut
  | UserDetailsEmailNoAuth


data RegisterError
  = RegisterErrorBadCaptchaResponse
  | RegisterErrorEmailInUse


data RedirectError
  = RedirectRegisterAuth
  | RedirectUserDetailsNoAuth


data SnackbarMessage
  = SnackbarMessageAuthFailure AuthTokenFailure
  | SnackbarMessageAuthError AuthError
  | SnackbarMessageUserDetails UserDetailsError
  | SnackbarMessageRegister (Maybe RegisterError)
  | SnackbarMessageRedirect RedirectError



type State =
  { errors :: List SnackbarMessage
  , open :: Boolean
  }

initialState :: State
initialState =
  { errors: Nil
  , open: false
  }

data Action
  = GotMessage SnackbarMessage
  | PopMessage
  | Open

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff. T.Spec (Effects eff) State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      GotMessage x -> do
        performAction Open props state
        void $ T.cotransform _ { errors = List.snoc state.errors x }
      PopMessage -> case List.uncons state.errors of
        Nothing -> pure unit -- bug out
        Just {head,tail} -> do
          liftBase $ delay $ Milliseconds $ 2000.0
          mState <- T.cotransform _ { errors = tail, open = false }
          unless (List.null tail) $ do
            liftBase $ delay $ Milliseconds $ 2000.0
            case mState of
              Nothing -> pure unit
              Just s -> performAction Open props s

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ snackbar
        { open: state.open
        , autoHideDuration: toNullable $ Just $ Milliseconds 10000.0
        -- , resumeHideDuration: toNullable $ Just $ Milliseconds 0.0
        , onClose: mkEffFn2 \_ _ -> do
            dispatch PopMessage
        , message: R.div []
          [ case List.head state.errors of
              Nothing -> R.text ""
              Just x -> case x of
                SnackbarMessageAuthFailure authFailure -> case authFailure of
                  BadPassword -> R.text "Password incorrect, please try again."
                  EmailDoesntExist -> R.text "Email address not found, please register."
                SnackbarMessageAuthError authError -> case authError of
                  FBLoginReturnBad code msg -> R.text $ "Bad Facebook login response: " <> code <> ", " <> msg
                  FBLoginReturnDenied desc -> R.text $ "Facebook login denied: " <> desc
                  FBLoginReturnBadParse -> R.text "Internal error: Facebook login return unparsable."
                  FBLoginReturnNoUser -> R.text "Facebook user not recognized, please link your account."
                  AuthExistsFailure -> R.text "Warning: You've been logged out; your session expired."
                SnackbarMessageUserDetails userDetails -> case userDetails of
                  UserDetailsEmailNoInitOut -> R.text "Internal Error: userDetails/email resource failed"
                  UserDetailsEmailNoAuth -> R.text "Error: No authorization for email"
                SnackbarMessageRegister mRegister -> case mRegister of
                  Nothing -> R.text "Registered! Please check your spam folder and confirm in 7 days."
                  Just register -> case register of
                    RegisterErrorBadCaptchaResponse -> R.text "Bad ReCaptcha response."
                    RegisterErrorEmailInUse -> R.text "Email address is already registered."
                SnackbarMessageRedirect redirect -> case redirect of
                  RedirectRegisterAuth -> R.text "Redirected - can't register while logged in."
                  RedirectUserDetailsNoAuth -> R.text "Redirected - can't view user details while logged out."
          ]
        }
      ]



messages :: forall eff
          . { errorMessageQueue :: One.Queue (read :: READ) (Effects eff) SnackbarMessage
            } -> R.ReactElement
messages {errorMessageQueue} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
      reactSpec' =
        Queue.whileMountedOne
          errorMessageQueue
          (\this x -> unsafeCoerceEff $ dispatcher this $ GotMessage x)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
