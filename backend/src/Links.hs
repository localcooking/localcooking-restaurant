{-# LANGUAGE
    MultiParamTypeClasses
  , RecordWildCards
  , OverloadedStrings
  , OverloadedLists
  #-}

module Links where

import Data.Monoid ((<>))
import Path.Extended (ToPath (..), ToLocation (..), Abs, File, fromPath, setFileExt, parseAbsFile)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.URI (URI (..))
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (..))
import qualified Data.Strict.Maybe as Strict
import Data.Strict.Tuple (Pair (..))


data LogoLinks
  = LogoPng
  | LogoWhitePng
  | IconPng
  | IconSvg


instance ToPath LogoLinks Abs File where
  toPath x = case x of
    LogoPng      -> parseAbsFile (parent <> "logo")
    LogoWhitePng -> parseAbsFile (parent <> "logo-white")
    IconPng      -> parseAbsFile (parent <> "icon")
    IconSvg      -> parseAbsFile (parent <> "icon")
    where
      parent = "/static/images/"


instance ToLocation LogoLinks Abs File where
  toLocation x = case x of
    LogoPng      -> (setFileExt (Just "png") . fromPath) <$> toPath x
    LogoWhitePng -> (setFileExt (Just "png") . fromPath) <$> toPath x
    IconPng      -> (setFileExt (Just "png") . fromPath) <$> toPath x
    IconSvg      -> (setFileExt (Just "svg") . fromPath) <$> toPath x


data FacebookLoginVerify = FacebookLoginVerify
  { facebookLoginVerifyClientID :: T.Text
  , facebookLoginVerifyRedirectURI :: URI
  , facebookLoginVerifyClientSecret :: T.Text
  , facebookLoginVerifyCode :: T.Text
  }


facebookLoginVerifyToURI :: FacebookLoginVerify -> URI
facebookLoginVerifyToURI FacebookLoginVerify{..} =
  URI
    (Strict.Just "https")
    True
    (URIAuth Strict.Nothing (Host ["graph", "facebook"] "com") Strict.Nothing)
    ["v2.12", "oauth", "access_token"]
    [ "client_id" :!: Strict.Just facebookLoginVerifyClientID
    , "redirect_uri" :!: Strict.Just (T.pack $ show facebookLoginVerifyRedirectURI)
    , "client_secret" :!: Strict.Just facebookLoginVerifyClientSecret
    , "code" :!: Strict.Just facebookLoginVerifyCode
    ]
    Strict.Nothing
