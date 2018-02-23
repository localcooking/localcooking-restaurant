module Spec.Dialogs.Login where

import Prelude
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)

import Thermite as T
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
  | eff)


spec :: forall eff
      . T.Spec eff State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> void $ T.cotransform _ { open = false }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ dialog
        { open: state.open
        , fullWidth: true
        , onClose: mkEffFn1 \_ -> dispatch Close
        }
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
            , onTouchTap: mkEffFn1 \_ -> dispatch Close
            } [R.text "Cancel"]
          ]
        ]
      ]



loginDialog :: forall eff
             . { openSignal :: Queue (read :: READ) (Effects eff) Unit
               }
            -> R.ReactElement
loginDialog {openSignal} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
      reactSpec' = whileMountedOne openSignal (\this _ -> unsafeCoerceEff $ dispatcher this Open) reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
