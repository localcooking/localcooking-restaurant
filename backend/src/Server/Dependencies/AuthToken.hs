{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , OverloadedLists
  #-}

module Server.Dependencies.AuthToken where

import Types (AppM)
import Types.Keys (Keys (..))
import Types.Env (Env (..), Devices (..), Managers (..), isDevelopment)
import Links (FacebookLoginVerify (..), facebookLoginVerifyToURI)
import Login.Facebook (FacebookLoginCode, FacebookLoginGetToken (..))
import LocalCooking.Password (HashedPassword)
import LocalCooking.Auth (AuthToken)
import LocalCooking.Email (Email)
import LocalCooking.Device (DeviceToken)

import Web.Dependencies.Sparrow (Server, ServerContinue (..), ServerReturn (..), ServerArgs (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), (.:), Value (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Strict.Maybe as Strict
import Data.Strict.Tuple (Pair (..))
import Data.Monoid ((<>))
import Data.URI (URI (..), printURI)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Exception.Safe (throwM)
import Control.Logging (log', warn')
import Network.HTTP.Client (httpLbs, responseBody, parseRequest)



-- TODO Google ReCaptcha
data AuthTokenInitIn
  = AuthTokenInitInLogin
    { authTokenInitInLoginEmail :: Email
    , authTokenInitInLoginPassword :: HashedPassword
    , authTokenInitInLoginDevice :: DeviceToken
    }
  | AuthTokenInitInFacebookCode
    { authTokenInitInFacebookCode :: FacebookLoginCode
    }
  | AuthTokenInitInExists
    { authTokenInitInExists :: AuthToken
    , authTokenInitInExistsDevice :: DeviceToken
    }


data AuthTokenFailure
  = DeviceTokenDoesntExist
  | BadPassword
  | EmailDoesntExist
  | BadFacebookLoginCode

data AuthTokenInitOut
  = AuthTokenInitOutSuccess AuthToken
  | AuthTokenInitOutFailure AuthTokenFailure


data AuthTokenDeltaIn
  = AuthTokenDeltaInLogout -- TODO plus AuthToken...? Tokens are --more-- mutually unique than SIDs?
    -- a session can die, but store the AuthToken in local storage and attempt to use later -
    -- login's discontinuity and session's discontinuity mutually overlay.


data AuthTokenDeltaOut
  = AuthTokenDeltaOutNew AuthToken
  | AuthTokenDeltaOutRevoked -- remotely logged out



authTokenServer :: Server AppM AuthTokenInitIn
                               AuthTokenInitOut
                               AuthTokenDeltaIn
                               AuthTokenDeltaOut
authTokenServer initIn = case initIn of
  AuthTokenInitInLogin email password deviceToken -> undefined
  AuthTokenInitInExists authToken deviceToken -> undefined
  -- invoked on facebookLoginReturn, only when the user exists
  AuthTokenInitInFacebookCode code -> do
    env@Env
      { envManagers = Managers{managersFacebook}
      , envKeys = Keys{keysFacebookClientID, keysFacebookClientSecret}
      , envHostname
      } <- ask

    let url = printURI $ facebookLoginVerifyToURI FacebookLoginVerify
          { facebookLoginVerifyClientID = keysFacebookClientID
          , facebookLoginVerifyClientSecret = keysFacebookClientSecret
          , facebookLoginVerifyRedirectURI = URI (Strict.Just "https") True envHostname ["facebookLoginReturn"] [] Strict.Nothing
          , facebookLoginVerifyCode = code
          }

    req' <- liftIO $ parseRequest (T.unpack url)
    resp' <- liftIO $ httpLbs req' managersFacebook

    case Aeson.decode (responseBody resp') of
      Nothing -> do
        throwM $ userError $ "Somehow couldn't parse facebook verify output: " <> show (responseBody resp')
        pure Nothing
      Just x -> do
        case x of
          FacebookLoginGetTokenError{} -> do
            when (isDevelopment env) $ warn' $
              "Couldn't verify facebook code due to formatting error: "
              <> T.pack (show x) <> ", from: " <> url
            throwM $ userError "Couldn't verify facebook code" -- FIXME
            pure Nothing
          FacebookLoginGetToken{facebookLoginGetTokenAccessToken} -> do
            log' $ "Got facebook access token: " <> T.pack (show x)
            pure Nothing
            -- FIXME ping facebook's graph API for details on token, get FB user's static userID
