module Client.Dependencies.Register where

import LocalCooking.Common.Password (HashedPassword)
import Google.ReCaptcha (ReCaptchaResponse)

import Prelude (bind, (<$>), unit, (==), pure, otherwise)

import Sparrow.Client.Queue (SparrowClientQueues)
import Text.Email.Validate (EmailAddress)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Text.Email.Validate as Email


newtype RegisterInitIn = RegisterInitIn
  { email :: EmailAddress
  , password :: HashedPassword
  , reCaptcha :: ReCaptchaResponse
  }

instance encodeJsonRegisterInitIn :: EncodeJson RegisterInitIn where
  encodeJson (RegisterInitIn {email,password,reCaptcha})
    =  "email" := Email.toString email
    ~> "password" := password
    ~> "reCaptcha" := reCaptcha
    ~> jsonEmptyObject

data RegisterFailure
  = EmailExists

instance decodeJsonRegisterFailure :: DecodeJson RegisterFailure where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "email-exists" -> pure EmailExists
        | otherwise -> fail "Not a RegisterFailure"


data RegisterInitOut
  = RegisterInitOutEmailSent
  | RegisterInitOutBadCaptcha
  | RegisterInitOutDBError RegisterFailure

instance decodeJsonRegisterInitOut :: DecodeJson RegisterInitOut where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          case unit of
            _ | s == "email-sent" -> pure RegisterInitOutEmailSent
              | s == "bad-captcha" -> pure RegisterInitOutBadCaptcha
              | otherwise -> fail "Not a RegisterInitOut"
        obj = do
          o <- decodeJson json
          RegisterInitOutDBError <$> o .? "db"
    str <|> obj

data RegisterDeltaIn

instance encodeJsonRegisterDeltaIn :: EncodeJson RegisterDeltaIn where
  encodeJson _ = encodeJson ""

data RegisterDeltaOut

instance decodeJsonRegisterDeltaOut :: DecodeJson RegisterDeltaOut where
  decodeJson json = fail "Not a RegisterDeltaOut"


type RegisterSparrowClientQueues eff =
  SparrowClientQueues eff RegisterInitIn RegisterInitOut RegisterDeltaIn RegisterDeltaOut
