{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , OverloadedLists
  #-}

module Server.Dependencies.AuthToken where

import Types (AppM)
import Types.Keys (Keys (..))
import Types.Env (Env (..), Managers (..), isDevelopment)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Database.Query.User (loginWithFB, usersAuthToken, login, logout, AuthTokenFailure)
import Text.EmailAddress (EmailAddress)
import Facebook.Types (FacebookLoginCode)
import Facebook.Return (handleFacebookLoginReturn)

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
import Network.HTTP.Types.URI (Query)
import Network.HTTP.Client (httpLbs, responseBody, parseRequest)



-- TODO Google ReCaptcha
data AuthTokenInitIn
  = AuthTokenInitInLogin
    { authTokenInitInLoginEmail :: EmailAddress
    , authTokenInitInLoginPassword :: HashedPassword
    }
  | AuthTokenInitInFacebookCode
    { authTokenInitInFacebookCode :: FacebookLoginCode
    }
  | AuthTokenInitInExists
    { authTokenInitInExists :: AuthToken
    }

instance FromJSON AuthTokenInitIn where
  parseJSON json = case json of
    Object o -> do
      let exists = AuthTokenInitInExists <$> o .: "exists"
          code = AuthTokenInitInFacebookCode <$> o .: "fbCode"
          login = AuthTokenInitInLogin <$> o .: "email" <*> o .: "password"
      exists <|> code <|> login
    _ -> fail
    where
      fail = typeMismatch "AuthTokenInitIn" json

data AuthTokenInitOut
  = AuthTokenInitOutSuccess AuthToken
  | AuthTokenInitOutFailure AuthTokenFailure

instance ToJSON AuthTokenInitOut where
  toJSON x = case x of
    AuthTokenInitOutFailure e -> object ["failure" .= e]
    AuthTokenInitOutSuccess y -> object ["success" .= y]


data AuthTokenDeltaIn
  = AuthTokenDeltaInLogout -- TODO plus AuthToken...? Tokens are --more-- mutually unique than SIDs?
    -- a session can die, but store the AuthToken in local storage and attempt to use later -
    -- login's discontinuity and session's discontinuity mutually overlay.

instance FromJSON AuthTokenDeltaIn where
  parseJSON json = case json of
    String x | x == "logout" -> pure AuthTokenDeltaInLogout
             | otherwise -> fail
    _ -> fail
    where
      fail = typeMismatch "AuthTokenDeltaIn" json


data AuthTokenDeltaOut
  = AuthTokenDeltaOutNew AuthToken
  | AuthTokenDeltaOutRevoked -- remotely logged out

instance ToJSON AuthTokenDeltaOut where
  toJSON x = case x of
    AuthTokenDeltaOutRevoked -> String "revoked"
    AuthTokenDeltaOutNew token -> object ["new" .= token]



authTokenServer :: Server AppM AuthTokenInitIn
                               AuthTokenInitOut
                               AuthTokenDeltaIn
                               AuthTokenDeltaOut
authTokenServer initIn = case initIn of
  -- invoked remotely from a client whenever casually attempting a normal login
  AuthTokenInitInLogin email password -> do
    Env{envDatabase} <- ask

    mToken <- liftIO $ login envDatabase email password

    case mToken of
      Left e -> pure $ Just ServerContinue
        { serverOnUnsubscribe = pure ()
        , serverContinue = \_ -> pure ServerReturn
          { serverInitOut = AuthTokenInitOutFailure e
          , serverOnOpen = \ServerArgs{serverDeltaReject} -> do
              serverDeltaReject
              pure Nothing
          , serverOnReceive = \_ _ -> pure ()
          }
        }
      Right authToken -> pure $ Just ServerContinue
        { serverOnUnsubscribe = pure ()
        , serverContinue = \_ -> pure ServerReturn
          { serverInitOut = AuthTokenInitOutSuccess authToken
          , serverOnOpen = \_ -> pure Nothing -- FIXME listen for remote logouts? Streaming auth tokens?
          , serverOnReceive = \_ r -> case r of
              AuthTokenDeltaInLogout -> liftIO $ logout envDatabase authToken
          }
        }

  -- invoked remotely from client when started with an authToken in frontendEnv, or in localStorage
  AuthTokenInitInExists authToken -> do
    Env{envDatabase} <- ask

    mUser <- liftIO $ usersAuthToken envDatabase authToken
    case mUser of
      Nothing -> pure Nothing
      Just _ -> pure $ Just ServerContinue
        { serverOnUnsubscribe = pure ()
        , serverContinue = \_ -> pure ServerReturn
          { serverInitOut = AuthTokenInitOutSuccess authToken
          , serverOnOpen = \_ -> pure Nothing -- FIXME listen for remote logouts? Streaming auth tokens?
          , serverOnReceive = \_ r -> case r of
              AuthTokenDeltaInLogout -> liftIO $ logout envDatabase authToken
          }
        }

  -- invoked on facebookLoginReturn, only when the user exists
  AuthTokenInitInFacebookCode code -> do
    env@Env
      { envManagers = Managers{managersFacebook}
      , envKeys = Keys{keysFacebook}
      , envHostname
      , envTls
      , envDatabase
      } <- ask

    eX <- liftIO $ handleFacebookLoginReturn managersFacebook keysFacebook envTls envHostname code
    case eX of
      Left e -> liftIO $ do
        putStr "Facebook error:"
        print e
        pure Nothing
      Right (fbToken,fbUserId) -> do
        mAuth <- liftIO $ loginWithFB envDatabase fbToken fbUserId
        case mAuth of
          Nothing -> pure Nothing
          Just authToken -> pure $ Just ServerContinue
            { serverOnUnsubscribe = pure ()
            , serverContinue = \_ -> pure ServerReturn
              { serverInitOut = AuthTokenInitOutSuccess authToken
              , serverOnOpen = \_ -> pure Nothing
              , serverOnReceive = \_ _ -> pure ()
              }
            }
