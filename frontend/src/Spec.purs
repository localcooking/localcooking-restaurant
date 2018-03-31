module Spec where

import Spec.Topbar (topbar)
import Spec.Content (content)
import Spec.Dialogs.Login (loginDialog)
import Spec.Drawers.LeftMenu (leftMenu)
import Spec.Snackbar (messages)
import Colors (palette)
import Window (WindowSize)
import Links (SiteLinks (RegisterLink))
import Login.Error (AuthError (AuthExistsFailure), PreliminaryAuthToken (..))
import Login.Storage (storeAuthToken, clearAuthToken)
import LocalCooking.Common.AuthToken (AuthToken)
import Client.Dependencies.AuthToken
  ( AuthTokenSparrowClientQueues, AuthTokenFailure
  , AuthTokenInitIn (..), AuthTokenInitOut (..))

import Prelude
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Control.Monad.Aff (makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Base (liftBase)

import Thermite as T
import React as R
import React.DOM as R
import MaterialUI.MuiThemeProvider (muiThemeProvider, createMuiTheme)
import MaterialUI.CssBaseline (cssBaseline)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)

import Queue (READ, WRITE)
import Queue.One.Aff as OneIO
import Queue.One as One
import IxSignal.Internal (IxSignal)



type State =
  { authToken :: Maybe AuthToken
  }


initialState :: State
initialState =
  { authToken: Nothing
  }


data Action
  = GotAuthToken AuthToken
  | ClearAuthToken
  | CallAuthToken AuthTokenInitIn


type Effects eff =
  ( ref        :: REF
  , exception  :: EXCEPTION
  , uuid       :: GENUUID
  , dom        :: DOM
  , history    :: HISTORY
  , now        :: NOW
  , timer      :: TIMER
  , webStorage :: WEB_STORAGE
  , console    :: CONSOLE
  , scrypt     :: SCRYPT
  | eff)

spec :: forall eff
      . { toURI              :: Location -> URI
        , windowSizeSignal   :: IxSignal (Effects eff) WindowSize
        , currentPageSignal  :: IxSignal (Effects eff) SiteLinks
        , siteLinks          :: SiteLinks -> Eff (Effects eff) Unit
        , development        :: Boolean
        , authTokenQueues    :: AuthTokenSparrowClientQueues (Effects eff)
        , authErrorSignal    :: One.Queue (read :: READ, write :: WRITE) (Effects eff) (Either AuthError AuthTokenFailure)
        , loginPendingSignal :: One.Queue (read :: READ, write :: WRITE) (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  { toURI
  , windowSizeSignal
  , siteLinks
  , currentPageSignal
  , development
  , authTokenQueues
  , authErrorSignal
  , loginPendingSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotAuthToken x   -> void $ T.cotransform _ { authToken = Just x }
      ClearAuthToken   -> void $ T.cotransform _ { authToken = Nothing }
      CallAuthToken initIn -> do
        initOut <- liftBase $ OneIO.callAsync authTokenQueues.init initIn
        case initOut of
          Nothing -> liftEff $ One.putQueue authErrorSignal (Left AuthExistsFailure)
          Just eInitOut -> do
            case eInitOut of
              AuthTokenInitOutSuccess authToken -> do
                liftEff $ storeAuthToken authToken
                performAction (GotAuthToken authToken) props state
              AuthTokenInitOutFailure e -> do
                liftEff $ One.putQueue authErrorSignal (Right e)
            liftEff $ One.putQueue loginPendingSignal unit


    render :: T.Render State Unit Action
    render dispatch props state children = template
      [ topbar
        { toURI
        , openLoginSignal: One.writeOnly openLoginSignal
        , windowSizeSignal
        , siteLinks
        , mobileMenuButtonSignal: One.writeOnly mobileMenuButtonSignal
        , currentPageSignal
        }
      , content
        { currentPageSignal
        }
      , loginDialog
        { openLoginSignal: One.readOnly openLoginSignal
        , windowSizeSignal
        , toURI
        , currentPageSignal
        , login: \email password -> makeAff \resolve -> do
            unsafeCoerceEff $ dispatch $ CallAuthToken $ AuthTokenInitInLogin {email,password}
            One.onQueue loginPendingSignal \_ -> resolve (Right unit)
            pure nonCanceler
        , toRegister: siteLinks RegisterLink
        }
      , leftMenu
        { mobileDrawerOpenSignal: One.readOnly mobileMenuButtonSignal
        , siteLinks
        }
      , messages
        { authErrorSignal: One.readOnly authErrorSignal
        }
      ]
      where
        template content =
          [ cssBaseline
          , muiThemeProvider
              { theme: createMuiTheme {palette}
              }
              (R.div [] content)
          ]

        openLoginSignal = unsafePerformEff One.newQueue
        mobileMenuButtonSignal = unsafePerformEff One.newQueue



app :: forall eff
     . { toURI                :: Location -> URI
       , windowSizeSignal     :: IxSignal (Effects eff) WindowSize
       , currentPageSignal    :: IxSignal (Effects eff) SiteLinks
       , siteLinks            :: SiteLinks -> Eff (Effects eff) Unit
       , development          :: Boolean
       , preliminaryAuthToken :: PreliminaryAuthToken
       , authTokenQueues      :: AuthTokenSparrowClientQueues (Effects eff)
       }
    -> { spec :: R.ReactSpec Unit State (Array R.ReactElement) (Effects eff)
       , dispatcher :: R.ReactThis Unit State -> Action -> T.EventHandler
       }
app
  { toURI
  , windowSizeSignal
  , currentPageSignal
  , siteLinks
  , development
  , preliminaryAuthToken
  , authTokenQueues
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
        ( spec
          { toURI
          , windowSizeSignal
          , currentPageSignal
          , siteLinks
          , development
          , authTokenQueues
          , authErrorSignal
          , loginPendingSignal
          }
        ) initialState
      reactSpec' = reactSpec
        { componentWillMount = \this -> do
          case preliminaryAuthToken of
            PreliminaryAuthToken Nothing -> pure unit
            PreliminaryAuthToken (Just eErr) -> case eErr of
              -- Call the authToken resource when the spec starts, using the preliminary
              -- auth token
              Right prescribedAuthToken ->
                unsafeCoerceEff $ dispatcher this $ CallAuthToken $
                  AuthTokenInitInExists {exists: prescribedAuthToken}
              Left e ->
                unsafeCoerceEff $ One.putQueue authErrorSignal (Left e)
          reactSpec.componentWillMount this
        }

  in  {spec: reactSpec', dispatcher}
  where
    authErrorSignal = unsafePerformEff One.newQueue
    loginPendingSignal = unsafePerformEff One.newQueue
