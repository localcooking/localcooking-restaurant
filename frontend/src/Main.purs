module Main where

import Links (SiteLinks (..), ImageLinks (Logo40Png), initSiteLinks)
import Types.Env (env)
import Colors (palette)
import Spec.Topbar.Buttons (topbarButtons)
import Spec.Content (content)
import LocalCooking.Links.Class (toLocation)
import LocalCooking.Branding.Main (mainBrand)
import LocalCooking.Main (defaultMain)
import LocalCooking.Spec.Icons.ChefHat (chefHatViewBox, chefHat)


import Prelude
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM)

import React as R
import React.DOM as R
import React.DOM.SVG as RS
import React.DOM.Props as RP
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT)
import MaterialUI.Divider (divider)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.SvgIcon (svgIcon)
import MaterialUI.SvgIcon as SvgIcon
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.Types (createStyles)
import MaterialUI.Icons.RestaurantMenu (restaurantMenuIcon)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)


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
  log "Starting Local Cooking Chefs frontend..."

  initSiteLink <- initSiteLinks

  let deps = do
        pure unit

  defaultMain
    { env
    , initSiteLinks: initSiteLink
    , palette
    , deps
    , leftDrawer:
      { buttons: \_ -> []
        -- [ divider {}
        -- , listItem
        --   { button: true
        --   , onClick: mkEffFn1 \_ -> unsafeCoerceEff $ siteLinks MealsLink
        --   }
        --   [ listItemIcon {} restaurantMenuIcon
        --   , listItemText
        --     { primary: "Meals"
        --     }
        --   ]
        -- , divider {}
        -- , listItem
        --   { button: true
        --   , onClick: mkEffFn1 \_ -> unsafeCoerceEff $ siteLinks ChefsLink
        --   }
        --   [ listItemIcon {} $ svgIcon {viewBox: chefHatViewBox, color: SvgIcon.action}
        --       [chefHat]
        --   , listItemText
        --     { primary: "Chefs"
        --     }
        --   ]
      }
    , topbar:
      { imageSrc: toLocation Logo40Png
      , buttons: \_ -> [] -- \{toURI,siteLinks,currentPageSignal,windowSizeSignal,authTokenSignal} ->
        -- [ topbarButtons
        --   { currentPageSignal
        --   , siteLinks
        --   , toURI
        --   }
        -- ]
      }
    , content: \{toURI,siteLinks,windowSizeSignal,currentPageSignal} ->
      [ content {toURI,siteLinks,windowSizeSignal,currentPageSignal} ]
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
            , color: SvgIcon.action
            , styles: createStyles {marginRight: "0.2em"}
            }
            [ mainBrand
            ]
          , R.text "Local Cooking"
          ]
      ]
    }
