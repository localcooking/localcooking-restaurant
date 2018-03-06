{-# LANGUAGE
    MultiParamTypeClasses
  , RecordWildCards
  , OverloadedStrings
  , OverloadedLists
  #-}

module Links where

import Data.Monoid ((<>))
import Path.Extended (ToPath (..), ToLocation (..), Abs, File, fromPath, setFileExt, addQuery, parseAbsFile, parseAbsDir)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text as T
import Data.URI (URI (..), printURI)
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (..))
import qualified Data.Strict.Maybe as Strict
import Data.Strict.Tuple (Pair (..))
import Crypto.Saltine.Core.Box (Nonce)
import qualified Crypto.Saltine.Class as NaCl
import Unsafe.Coerce (unsafeCoerce)


data WebAssetLinks
  = IndexCss -- FIXME Cache buster
  | IndexJs (Maybe Nonce)

instance ToPath WebAssetLinks Abs File where
  toPath x = case x of
    IndexCss -> parseAbsFile "/index"
    IndexJs _ -> parseAbsFile "/index"

instance ToLocation WebAssetLinks Abs File where
  toLocation x = case x of
    IndexCss -> (setFileExt (Just "css") . fromPath) <$> toPath x
    IndexJs mNonce ->
      ( setFileExt (Just "js")
      . ( case mNonce of
            Nothing -> id
            Just nonce -> addQuery ("cache_buster", Just $ BS8.toString $ BS16.encode $ NaCl.encode nonce)
        )
      . fromPath
      ) <$> toPath x


data SiteLinks
  = RootLink
  | AboutLink

instance ToPath SiteLinks Abs File where
  toPath x = case x of
    RootLink -> unsafeCoerce <$> parseAbsDir "/"
    AboutLink -> parseAbsFile "/about"

instance ToLocation SiteLinks Abs File where
  toLocation x = fromPath <$> toPath x


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
    , "redirect_uri" :!: Strict.Just (printURI facebookLoginVerifyRedirectURI)
    , "client_secret" :!: Strict.Just facebookLoginVerifyClientSecret
    , "code" :!: Strict.Just facebookLoginVerifyCode
    ]
    Strict.Nothing
