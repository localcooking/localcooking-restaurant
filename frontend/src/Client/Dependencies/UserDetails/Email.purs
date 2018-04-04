module Client.Dependencies.UserDetails.Email where

import LocalCooking.Common.AuthToken (AuthToken)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Text.Email.Validate (EmailAddress, emailAddress)
import Control.Alternative ((<|>))
import Sparrow.Client.Queue (SparrowClientQueues)



newtype UserDetailsEmailInitIn = UserDetailsEmailInitIn AuthToken

instance encodeJsonUserDetailsEmailInitIn :: EncodeJson UserDetailsEmailInitIn where
  encodeJson (UserDetailsEmailInitIn x) = "authToken" := x ~> jsonEmptyObject

data UserDetailsEmailInitOut
  = UserDetailsEmailInitOutNoAuth
  | UserDetailsEmailInitOutSuccess EmailAddress

instance decodeJsonUserDetailsEmailInitOut :: DecodeJson UserDetailsEmailInitOut where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "no-auth" -> pure UserDetailsEmailInitOutNoAuth
              | otherwise -> fail "Not a UserDetailsEmailInitOut"
        obj = do
          o <- decodeJson json
          e <- o .? "email"
          case emailAddress e of
            Nothing -> fail "Not a UserDetailsEmailInitOutSuccess"
            Just email -> pure (UserDetailsEmailInitOutSuccess email)
    str <|> obj

data UserDetailsEmailDeltaIn

instance encodeJsonUserDetailsEmailDeltaIn :: EncodeJson UserDetailsEmailDeltaIn where
  encodeJson _ = encodeJson ""

data UserDetailsEmailDeltaOut

instance decodeJsonUserDetailsEmailDeltaOut :: DecodeJson UserDetailsEmailDeltaOut where
  decodeJson _ = fail "Not a UserDetailsEmailDeltaOut"


type UserDetailsEmailSparrowClientQueues eff =
  SparrowClientQueues eff UserDetailsEmailInitIn UserDetailsEmailInitOut UserDetailsEmailDeltaIn UserDetailsEmailDeltaOut
