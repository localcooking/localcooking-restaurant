module LocalCooking.Subs where

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (.?), (~>), (:=), jsonEmptyObject, fail)
import Control.Alternative ((<|>))



data SubsInput

instance encodeJsonSubsInput :: EncodeJson SubsInput where
  encodeJson x = encodeJson "D:" -- case x of
    -- CandleStickInput x' -> encodeJson x'


data SubsOutput

instance decodeJsonSubsOutput :: DecodeJson SubsOutput where
  decodeJson json = fail "D:" -- (CandleStickOutput <$> decodeJson json)
