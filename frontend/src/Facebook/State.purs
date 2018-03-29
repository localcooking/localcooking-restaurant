module Facebook.State where

import Links (SiteLinks)

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?), fail)


newtype FacebookLoginState = FacebookLoginState
  { origin :: SiteLinks
  }

instance encodeJsonFacebookLoginState :: EncodeJson FacebookLoginState where
  encodeJson (FacebookLoginState {origin})
    =  "origin" := origin
    ~> jsonEmptyObject

instance decodeJsonFacebookLoginState :: DecodeJson FacebookLoginState where
  decodeJson json = do
    o <- decodeJson json
    origin <- o .? "origin"
    pure $ FacebookLoginState {origin}
