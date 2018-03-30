module Test.Main where

import Login.Error (AuthError)
import Links (SiteLinks)
import Facebook.State (FacebookLoginState)

import Prelude
import Data.Either (Either (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.QuickCheck (quickCheck)



main :: Eff _ Unit
main = do
  log "JSON Iso:"
  log "    AuthError:"
  quickCheck (\(x :: AuthError) -> jsonIso x)
  log ""
  log "    SiteLinks:"
  quickCheck (\(x :: SiteLinks) -> jsonIso x)
  log ""
  log "    FacebookLoginState:"
  quickCheck (\(x :: FacebookLoginState) -> jsonIso x)



jsonIso :: forall a. EncodeJson a => DecodeJson a => Eq a => a -> Boolean
jsonIso x = case decodeJson (encodeJson x) of
  Left _ -> false
  Right y -> x == y
