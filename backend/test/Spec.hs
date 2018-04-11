{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}

import LocalCooking.Login.Error (AuthError, PreliminaryAuthToken)
import Links (SiteLinks)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Path.Extended (ToLocation (..), FromLocation (..))


main :: IO ()
main = defaultMain $ testGroup "LocalCooking.com tests"
  [ testGroup "JSON encodings"
    [ testProperty "AuthError" (\(x :: AuthError) -> jsonIso x)
    , testProperty "PreliminaryAuthToken" (\(x :: PreliminaryAuthToken) -> jsonIso x)
    ]
  , testGroup "Location encodings"
    [ testProperty "SiteLinks" (\(x :: SiteLinks) -> locationIso x)
    ]
  ]


jsonIso :: FromJSON a => ToJSON a => Eq a => a -> Bool
jsonIso x = case Aeson.decode (Aeson.encode x) of
  Nothing -> False
  Just y -> x == y


locationIso :: FromLocation a => ToLocation a => Eq a => a -> Bool
locationIso x = case parseLocation (toLocation x) of
  Left _ -> False
  Right y -> x == y
