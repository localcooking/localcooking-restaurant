{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module Server.Dependencies.UserDetails.Email where

import Types (AppM)
import Types.Env (Env (..))
import LocalCooking.Auth (usersAuthToken)
import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Database.Query.User (getEmail)

import Web.Dependencies.Sparrow (Server, ServerReturn (..), ServerContinue (..), ServerArgs (..))

import Text.EmailAddress (EmailAddress)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, (.=), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)


newtype UserDetailsEmailInitIn = UserDetailsEmailInitIn
  { userDetailsEmailInitInAuthToken :: AuthToken
  }

instance FromJSON UserDetailsEmailInitIn where
  parseJSON json = case json of
    Object o -> UserDetailsEmailInitIn <$> o .: "authToken"
    _ -> fail
    where
      fail = typeMismatch "UserDetailsEmailInitIn" json


data UserDetailsEmailInitOut
  = UserDetailsEmailInitOutNoAuth
  | UserDetailsEmailInitOutSuccess EmailAddress

instance ToJSON UserDetailsEmailInitOut where
  toJSON x = case x of
    UserDetailsEmailInitOutNoAuth -> String "no-auth"
    UserDetailsEmailInitOutSuccess x -> object ["email" .= x]


data UserDetailsEmailDeltaIn

instance FromJSON UserDetailsEmailDeltaIn where
  parseJSON = typeMismatch "UserDetailsEmailDeltaIn"


data UserDetailsEmailDeltaOut

instance ToJSON UserDetailsEmailDeltaOut where
  toJSON _ = String ""



userDetailsEmailServer :: Server AppM UserDetailsEmailInitIn
                                      UserDetailsEmailInitOut
                                      UserDetailsEmailDeltaIn
                                      UserDetailsEmailDeltaOut
userDetailsEmailServer (UserDetailsEmailInitIn authToken) = do
  Env{envDatabase} <- ask

  mEmail <- do
    mUserId <- usersAuthToken authToken
    case mUserId of
      Nothing -> pure Nothing
      Just userId -> liftIO $ getEmail envDatabase userId

  case mEmail of
    Nothing -> pure $ Just ServerContinue
      { serverOnUnsubscribe = pure ()
      , serverContinue = \_ -> pure ServerReturn
        { serverInitOut = UserDetailsEmailInitOutNoAuth
        , serverOnOpen = \ServerArgs{serverDeltaReject} -> do
            serverDeltaReject
            pure Nothing
        , serverOnReceive = \_ _ -> pure ()
        }
      }
    Just email -> pure $ Just ServerContinue
      { serverOnUnsubscribe = pure ()
      , serverContinue = \_ -> pure ServerReturn
        { serverInitOut = UserDetailsEmailInitOutSuccess email
        , serverOnOpen = \ServerArgs{serverDeltaReject} -> do
            serverDeltaReject
            pure Nothing
        , serverOnReceive = \_ _ -> pure ()
        }
      }
