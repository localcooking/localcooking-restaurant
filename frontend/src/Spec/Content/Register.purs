module Spec.Content.Register where

import Types.Env (env)

import Prelude
import Data.Maybe (Maybe (..))
import Text.Email.Validate (emailAddress)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (mkEffFn1)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.ReCaptcha (reCaptcha, ReCaptchaResponse)

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.Divider (divider)
import MaterialUI.TextField (textField)
import MaterialUI.Input as Input
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid

import Unsafe.Coerce (unsafeCoerce)


type State =
  { reCaptcha :: Maybe ReCaptchaResponse
  , email :: String
  , emailDirty :: Maybe Boolean
  , emailConfirm :: String
  , emailConfirmDirty :: Maybe Boolean
  , password :: String
  , passwordDirty :: Maybe Boolean
  , passwordConfirm :: String
  , passwordConfirmDirty :: Maybe Boolean
  }

initialState :: State
initialState =
  { reCaptcha: Nothing
  , email: ""
  , emailDirty: Nothing
  , emailConfirm: ""
  , emailConfirmDirty: Nothing
  , password: ""
  , passwordDirty: Nothing
  , passwordConfirm: ""
  , passwordConfirmDirty: Nothing
  }

data Action
  = GotReCaptchaVerify ReCaptchaResponse
  | ChangedEmail String
  | ChangedEmailConfirm String
  | EmailUnfocused
  | EmailConfirmUnfocused
  | ChangedPassword String
  | ChangedPasswordConfirm String
  | PasswordUnfocused
  | PasswordConfirmUnfocused
  | SubmitRegister


type Effects eff =
  ( console :: CONSOLE
  | eff)


spec :: forall eff. T.Spec (Effects eff) State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotReCaptchaVerify e -> void $ T.cotransform _ { reCaptcha = Just e }
      ChangedEmail e -> void $ T.cotransform _ { email = e, emailDirty = Just false }
      ChangedEmailConfirm e -> void $ T.cotransform _ { emailConfirm = e, emailConfirmDirty = Just false }
      ChangedPassword e -> void $ T.cotransform _ { password = e, passwordDirty = Just false }
      ChangedPasswordConfirm e -> void $ T.cotransform _ { passwordConfirm = e, passwordConfirmDirty = Just false }
      EmailUnfocused -> void $ T.cotransform _ { emailDirty = Just true }
      EmailConfirmUnfocused -> void $ T.cotransform _ { emailConfirmDirty = Just true }
      PasswordUnfocused -> void $ T.cotransform _ { passwordDirty = Just true }
      PasswordConfirmUnfocused -> void $ T.cotransform _ { passwordConfirmDirty = Just true }
      SubmitRegister -> pure unit


    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.display2
        , align: Typography.center
        , color: Typography.primary
        } [R.text "Register"]
      , R.div [RP.style {marginBotton: "1em"}] []
      , divider {}
      , grid
        { spacing: Grid.spacing8
        , container: true
        , justify: Grid.centerJustify
        }
        [ grid
          { xs: 6
          , item: true
          }
          [ textField
            { label: R.text "Email"
            , fullWidth: true
            , onChange: mkEffFn1 \e -> dispatch $ ChangedEmail (unsafeCoerce e).target.value
            , onBlur: mkEffFn1 \_ -> dispatch EmailUnfocused
            , error: case emailAddress state.email of
                Nothing -> state.emailDirty == Just true
                Just _ -> case state.emailConfirmDirty of
                  Nothing -> false
                  Just dirty
                    | dirty -> state.email /= state.emailConfirm
                    | otherwise -> false
            , name: "register-email"
            , id: "register-email"
            }
          , textField
            { label: R.text "Email Confirm"
            , fullWidth: true
            , onChange: mkEffFn1 \e -> dispatch $ ChangedEmailConfirm (unsafeCoerce e).target.value
            , onBlur: mkEffFn1 \_ -> dispatch EmailConfirmUnfocused
            , error: case emailAddress state.emailConfirm of
                Nothing -> state.emailConfirmDirty == Just true
                Just _ -> case state.emailDirty of
                  Nothing -> false
                  Just _ -> state.email /= state.emailConfirm
            , name: "register-email-confirm"
            , id: "register-email-confirm"
            }
          , textField
            { label: R.text "Password"
            , fullWidth: true
            , "type": Input.passwordType
            , onChange: mkEffFn1 \p -> dispatch $ ChangedPassword (unsafeCoerce p).target.value
            , onBlur: mkEffFn1 \_ -> dispatch PasswordUnfocused
            , error: case state.passwordDirty of
                Nothing -> false
                Just dirty
                  | not dirty -> false
                  | otherwise -> case state.passwordConfirmDirty of
                    Nothing -> false
                    Just dirty
                      | not dirty -> false
                      | otherwise -> state.password /= state.passwordConfirm
            , name: "register-password"
            , id: "register-password"
            }
          , textField
            { label: R.text "Password Confirm"
            , fullWidth: true
            , "type": Input.passwordType
            , onChange: mkEffFn1 \p -> dispatch $ ChangedPasswordConfirm (unsafeCoerce p).target.value
            , onBlur: mkEffFn1 \_ -> dispatch PasswordConfirmUnfocused
            , error: case state.passwordConfirmDirty of
                Nothing -> false
                Just dirty
                  | not dirty -> false
                  | otherwise -> case state.passwordDirty of
                    Nothing -> false
                    Just dirty
                      | not dirty -> false
                      | otherwise -> state.password /= state.passwordConfirm
            , name: "register-password-confirm"
            , id: "register-password-confirm"
            }
          , R.div [RP.style {marginBotton: "1em"}] []
          , reCaptcha
            { sitekey: env.googleReCaptchaSiteKey
            , verifyCallback: mkEffFn1 (dispatch <<< GotReCaptchaVerify)
            , onloadCallback: pure unit
            }
          , button
            { color: Button.secondary
            , variant: Button.raised
            , size: Button.large
            , style: createStyles {marginTop: "1em"}
            , disabled: case emailAddress state.email of
              Nothing -> true
              Just _ -> state.email /= state.emailConfirm
                    || state.password /= state.passwordConfirm
                    || case state.passwordDirty of
                          Nothing -> true
                          Just _ -> case state.reCaptcha of
                            Nothing -> true
                            Just _ -> false
            , onTouchTap: mkEffFn1 \_ -> dispatch SubmitRegister
            } [R.text "Submit"]
          ]
        ]
      ]


register :: R.ReactElement
register =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []
