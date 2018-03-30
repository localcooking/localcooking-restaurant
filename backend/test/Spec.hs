{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}

import Login.Error (AuthError, PreliminaryAuthToken)
import Links (SiteLinks)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson


main :: IO ()
main = defaultMain $ testGroup "JSON encodings"
  [ testProperty "AuthError" (\(x :: AuthError) -> jsonIso x)
  , testProperty "PreliminaryAuthToken" (\(x :: PreliminaryAuthToken) -> jsonIso x)
  , testProperty "SiteLinks" (\(x :: SiteLinks) -> jsonIso x)
  ]


jsonIso :: FromJSON a => ToJSON a => Eq a => a -> Bool
jsonIso x = case Aeson.decode (Aeson.encode x) of
  Nothing -> False
  Just y -> x == y
