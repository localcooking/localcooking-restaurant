module Login.Error where

import LocalCooking.Common.AuthToken (AuthToken)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut (class DecodeJson, decodeJson, (.?), fail)
import Control.Alternative ((<|>))


data AuthError
  = FBLoginReturnBad String String
  | FBLoginReturnDenied String
  | FBLoginReturnBadParse
  | FBLoginReturnNoUser
  | AuthExistsFailure


instance decodeJsonAuthError :: DecodeJson AuthError where
  decodeJson json = do
    let obj = do
          o <- decodeJson json
          let bad = do
                o' <- o .? "fbBad"
                FBLoginReturnBad <$> o' .? "code" <*> o' .? "msg"
              denied = do
                o' <- o .? "fbDenied"
                FBLoginReturnDenied <$> o' .? "desc"
          bad <|> denied
        str = do
          s <- decodeJson json
          case unit of
            _ | s == "bad-parse" -> pure FBLoginReturnBadParse
              | s == "no-user" -> pure FBLoginReturnNoUser
              | s == "exists-failure" -> pure AuthExistsFailure
              | otherwise -> fail "Not a AuthError"
    obj <|> str



newtype PreliminaryAuthToken = PreliminaryAuthToken
  (Maybe (Either AuthError AuthToken))

instance decodeJsonPreliminaryAuthToken :: DecodeJson PreliminaryAuthToken where
  decodeJson json = do
    mO <- decodeJson json
    case mO of
      Nothing -> pure (PreliminaryAuthToken Nothing)
      Just o -> do
        let err = Left <$> o .? "err"
            token = Right <$> o .? "token"
        (PreliminaryAuthToken <<< Just) <$> (err <|> token)
