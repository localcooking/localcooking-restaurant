module Main where

import Colors (palette)
import User (UserDetails (..), PreUserDetails (..))
import Spec.Topbar.Buttons (topbarButtons)
import Spec.Drawers.Buttons (drawersButtons)
import Spec.Content (content)
import Spec.Content.UserDetails (userDetails)
import Spec.Content.UserDetails.Buttons (userDetailsButtons)
import Spec.Snackbar (messages)
import LocalCooking.Types.ServerToClient (env)
import LocalCooking.Main (defaultMain)
import LocalCooking.Spec.Misc.Branding (mainBrand)
import LocalCooking.Dependencies.Restaurant (restaurantDependencies, newRestaurantQueues)
import LocalCooking.Dependencies.Tag (tagDependencies, newTagQueues, mountTagSearchQueues)
import LocalCooking.Global.Links.Internal (ImageLinks (Logo40Png))

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI.Location (toLocation)
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Control.Monad.Aff (sequential)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM)

import React.DOM (text) as R
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.SvgIcon (svgIcon)
import MaterialUI.SvgIcon as SvgIcon
import MaterialUI.Types (createStyles)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)
import Queue.Types (readOnly, writeOnly)
import Queue.One as One



-- | All top-level effects
type Effects =
  ( console            :: CONSOLE
  , injectTapEvent     :: INJECT_TAP_EVENT
  , set_immediate_shim :: SET_IMMEDIATE_SHIM
  , ref                :: REF
  , dom                :: DOM
  , timer              :: TIMER
  , uuid               :: GENUUID
  , exception          :: EXCEPTION
  , history            :: HISTORY
  , now                :: NOW
  , ws                 :: WEBSOCKET
  , ajax               :: AJAX
  , webStorage         :: WEB_STORAGE
  , scrypt             :: SCRYPT
  )


main :: Eff Effects Unit
main = do
  log "Starting Local Cooking Restaurtants frontend..."

  restaurantQueues <- newRestaurantQueues
  tagQueues <- newTagQueues
  siteErrorQueue <- One.newQueue

  tagSearch <- mountTagSearchQueues tagQueues
    { onChefTagSearchResult: \_ -> pure unit
    , onCultureTagSearchResult: \_ -> pure unit
    , onDietTagSearchResult: \_ -> pure unit
    , onFarmTagSearchResult: \_ -> pure unit
    , onIngredientTagSearchResult: \_ -> pure unit
    , onMealTagSearchResult: \_ -> pure unit
    }

  defaultMain
    { env
    , palette
    , deps: do
        restaurantDependencies restaurantQueues
        tagDependencies tagQueues
    , extraRedirect: \_ _ -> Nothing
    , leftDrawer:
      { buttons: drawersButtons
      }
    , topbar:
      { imageSrc: toLocation Logo40Png
      , buttons: topbarButtons
      }
    , content: content
    , userDetails:
      { buttons: userDetailsButtons
      , content: userDetails
      , obtain: \{user} -> do
        PreUserDetails mUser <- sequential $ PreUserDetails <$> user
        case mUser of
          Just user -> pure $ Just $ UserDetails {user}
          _ -> pure Nothing
      }
    , error:
      { content: messages {siteErrorQueue: readOnly siteErrorQueue}
      }
    , extendedNetwork:
      [ Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#c62828"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#ff5f52"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Customers"
          ]
      , R.text " "
      , Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#1565c0"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#5e92f3"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://chef.localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Chefs"
          ]
      , R.text " "
      , Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#1b5e20"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#4c8c4a"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://farm.localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Farms"
          ]
      ]
    }
