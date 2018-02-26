module LocalCooking.WebSocket where

import LocalCooking.Subs (SubsInput, SubsOutput)
import LocalCooking.Auth (UserID, SignedChallenge)

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (.?), (~>), (:=), jsonEmptyObject, fail)
import Control.Alternative ((<|>))


data LocalCookingOp
  = LocalCookingOpChartData

instance encodeJsonLocalCookingOp :: EncodeJson LocalCookingOp where
  encodeJson LocalCookingOpChartData = encodeJson "chartData"


data LocalCookingInput
  = LocalCookingLogin {userID :: UserID, signedChallenge :: SignedChallenge}
  -- | LocalCookingOp LocalCookingOp
  | LocalCookingSubsInput SubsInput

instance encodeJsonLocalCookingInput :: EncodeJson LocalCookingInput where
  encodeJson x = case x of
    LocalCookingLogin {userID,signedChallenge}
      ->  "login" :=
          ( "userID" := userID
          ~> "signedChallenge" := signedChallenge
          ~> jsonEmptyObject)
      ~> jsonEmptyObject
    -- LocalCookingOp o
    --   ->  "op" := o
    --   ~> jsonEmptyObject
    LocalCookingSubsInput y
      ->  encodeJson y


data LocalCookingLoginResult
  = LocalCookingLoginSuccess
  | LocalCookingLoginFailure

instance decodeJsonLocalCookingLoginResult :: DecodeJson LocalCookingLoginResult where
  decodeJson json = do
    r <- decodeJson json
    case r of
      _ | r == "success" -> pure LocalCookingLoginSuccess
        | r == "failure" -> pure LocalCookingLoginFailure
        | otherwise -> fail "Not a LocalCookingLoginResult"


data LocalCookingOutput
  = LocalCookingLoginResult LocalCookingLoginResult
  -- | LocalCookingOpResult LocalCookingOpResult
  | LocalCookingSubsOutput SubsOutput

instance decodeJsonLocalCookingOutput :: DecodeJson LocalCookingOutput where
  decodeJson json = do
    o <- decodeJson json
    let decodeLoginResult = LocalCookingLoginResult <$> o .? "loginResult"
        -- decodeOpResult = LocalCookingOpResult <$> o .? "op"
        decodeSubsOutput = LocalCookingSubsOutput <$> decodeJson json
    decodeLoginResult <|> decodeSubsOutput
