{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module Server.Dependencies.UserDetails.Email where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Auth (usersAuthToken)
import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Database.Query.User (getEmail)

import Web.Dependencies.Sparrow.Types (Server, JSONVoid, staticServer)

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
    UserDetailsEmailInitOutSuccess y -> object ["email" .= y]




userDetailsEmailServer :: Server AppM UserDetailsEmailInitIn
                                      UserDetailsEmailInitOut
                                      JSONVoid
                                      JSONVoid
userDetailsEmailServer = staticServer $ \(UserDetailsEmailInitIn authToken) -> do
  Env{envDatabase} <- ask

  mEmail <- do
    mUserId <- usersAuthToken authToken
    case mUserId of
      Nothing -> pure Nothing
      Just userId -> liftIO $ getEmail envDatabase userId

  case mEmail of
    Nothing -> pure $ Just  UserDetailsEmailInitOutNoAuth
    Just email -> pure $ Just $ UserDetailsEmailInitOutSuccess email
