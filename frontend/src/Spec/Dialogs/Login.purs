module Spec.Dialogs.Login where

import Window (WindowSize (..))

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import Thermite.Window as WindowT
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted (whileMountedOne)
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
  }


initialState :: State
initialState =
  { open: false
  }


data Action
  = Open
  | Close

type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  | eff)


spec :: forall eff
      . T.Spec eff (WindowT.State State) Unit (WindowT.Action Action)
spec = T.simpleSpec (WindowT.performAction performAction) render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> void $ T.cotransform _ { open = false }

    render :: T.Render (WindowT.State State) Unit (WindowT.Action Action)
    render dispatch props {windowSize,state} children =
      [ let dialog' =
              if windowSize < Laptop
              then
                dialog
                  { open: state.open
                  , fullWidth: true
                  }
              else
                dialog
                  { open: state.open
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ -> dispatch $ WindowT.Action Close
                  }
        in  dialog'
            [ dialogTitle {} [R.text "Login"]
            , dialogContent {}
              [ textField {label: R.text "Email", fullWidth: true}
              , textField {label: R.text "Password", fullWidth: true, "type": Input.passwordType}
              , R.div [RP.style {display: "flex", justifyContent: "space-evenly", paddingTop: "2em"}] $
                  let mkFab mainColor darkColor icon =
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
                              } [icon]
                          )
                  in  [ mkFab "#3b5998" "#1e3f82" facebookIcon
                      , mkFab "#1da1f3" "#0f8cdb" twitterIcon
                      , mkFab "#dd4e40" "#c13627" googleIcon
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
                , onTouchTap: mkEffFn1 \_ -> dispatch $ WindowT.Action Close
                } [R.text "Cancel"]
              ]
            ]
      ]



loginDialog :: forall eff
             . { openSignal :: Queue (read :: READ) (Effects eff) Unit
               , windowSizeSignal :: IxSignal (Effects eff) WindowSize
               }
            -> R.ReactElement
loginDialog {openSignal,windowSizeSignal} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec (WindowT.initialState initialState)
      reactSpec' = whileMountedOne openSignal (\this _ -> unsafeCoerceEff $ dispatcher this $ WindowT.Action Open)
                 $ WindowT.listening windowSizeSignal {spec: reactSpec, dispatcher}
  in  R.createElement (R.createClass reactSpec') unit []
