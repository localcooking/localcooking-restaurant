module Spec.Dialogs.Login where

import Window (WindowSize (..), initialWindowSize)
import Links (SiteLinks (..), ThirdPartyLoginReturnLinks (..), toLocation)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal
import React.Icons (facebookIcon, twitterIcon, googleIcon)

import MaterialUI.Types (createStyles)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Dialog (dialog)
import MaterialUI.Dialog as Dialog
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.TextField (textField)
import MaterialUI.TextField as TextField
import MaterialUI.Input as Input

import Queue.One (READ, Queue)
import IxSignal.Internal (IxSignal)



type State =
  { open :: Boolean
  , windowSize :: WindowSize
  }


initialState :: State
initialState =
  { open: false
  , windowSize: unsafePerformEff initialWindowSize
  }


data Action
  = Open
  | Close
  | ChangedWindowSize WindowSize

type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  | eff)


spec :: forall eff
      . { toURI :: Location -> URI
        }
     -> T.Spec eff State Unit Action
spec {toURI} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> void $ T.cotransform _ { open = false }
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ let dialog' =
              if state.windowSize < Laptop
              then
                dialog
                  { open: state.open
                  , fullScreen: true
                  }
              else
                dialog
                  { open: state.open
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ -> dispatch Close
                  }
        in  dialog'
            [ dialogTitle {} [R.text "Login"]
            , dialogContent {}
              [ textField {label: R.text "Email", fullWidth: true}
              , textField {label: R.text "Password", fullWidth: true, "type": Input.passwordType}
              , R.div [RP.style {display: "flex", justifyContent: "space-evenly", paddingTop: "2em"}] $
                  let mkFab mainColor darkColor icon mLink =
                        Button.withStyles
                          (\theme ->
                            { root: createStyles
                              { backgroundColor: mainColor
                              , color: "#ffffff"
                              , "&:hover": {backgroundColor: darkColor}
                              }
                            }
                          )
                          (\{classes} ->
                            button
                              { variant: Button.fab
                              , classes: Button.createClasses {root: classes.root}
                              , disabled: case mLink of
                                Nothing -> true
                                _ -> false
                              , href: case mLink of
                                Nothing -> ""
                                Just link -> URI.print $ facebookLoginLinkToURI link
                              } [icon]
                          )
                  in  [ mkFab "#3b5998" "#1e3f82" facebookIcon $
                         Just $ FacebookLoginLink
                         { redirectURL: toURI (toLocation FacebookLoginReturn)
                         , state: unit
                         }
                      , mkFab "#1da1f3" "#0f8cdb" twitterIcon Nothing
                      , mkFab "#dd4e40" "#c13627" googleIcon Nothing
                      ]
              ]
            , dialogActions {}
              [ button
                { color: Button.secondary
                -- , onTouchTap: mkEffFn1 \_ -> dispatch Close
                } [R.text "Register"]
              , button
                { color: Button.primary
                -- , onTouchTap: mkEffFn1 \_ -> dispatch Close
                } [R.text "Submit"]
              , button
                { color: Button.default
                , onTouchTap: mkEffFn1 \_ -> dispatch Close
                } [R.text "Cancel"]
              ]
            ]
      ]



loginDialog :: forall eff
             . { openLoginSignal :: Queue (read :: READ) (Effects eff) Unit
               , windowSizeSignal :: IxSignal (Effects eff) WindowSize
               , toURI :: Location -> URI
               }
            -> R.ReactElement
loginDialog {openLoginSignal,windowSizeSignal,toURI} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec {toURI}) initialState
      reactSpecLogin =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Queue.whileMountedOne
            openLoginSignal
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
            reactSpec
  in  R.createElement (R.createClass reactSpecLogin) unit []
