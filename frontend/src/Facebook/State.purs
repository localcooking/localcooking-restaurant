module Facebook.State where

import Links (SiteLinks)

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?), fail)
import Data.Generic (class Generic, gEq, gShow)
import Test.QuickCheck (class Arbitrary, arbitrary)


newtype FacebookLoginState = FacebookLoginState
  { origin :: SiteLinks
  }

derive instance genericFacebookLoginState :: Generic FacebookLoginState

instance eqFacebookLoginState :: Eq FacebookLoginState where
  eq = gEq

instance arbitraryFacebookLoginState :: Arbitrary FacebookLoginState where
  arbitrary = do
    origin <- arbitrary
    pure $ FacebookLoginState {origin}

instance encodeJsonFacebookLoginState :: EncodeJson FacebookLoginState where
  encodeJson (FacebookLoginState {origin})
    =  "origin" := origin
    ~> jsonEmptyObject

instance decodeJsonFacebookLoginState :: DecodeJson FacebookLoginState where
  decodeJson json = do
    o <- decodeJson json
    origin <- o .? "origin"
    pure $ FacebookLoginState {origin}
