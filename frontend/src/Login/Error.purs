module Login.Error where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.?), fail)
import Control.Alternative ((<|>))


data AuthError
  = FBLoginReturnBad String String
  | FBLoginReturnDenied String
  | FBLoginReturnBadParse
  | FBLoginReturnNoUser


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
              | otherwise -> fail "Not a AuthError"
    obj <|> str
