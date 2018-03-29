{-# LANGUAGE
    MultiParamTypeClasses
  , RecordWildCards
  , OverloadedStrings
  , OverloadedLists
  #-}

module Links where

import Data.Monoid ((<>))
import Path.Extended (ToPath (..), ToLocation (..), Abs, File, fromPath, setFileExt, addQuery, parseAbsFile, parseAbsDir)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
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

-- TODO attoparsec parser? URI / Location parser...?

instance ToJSON SiteLinks where
  toJSON x = String $ case x of
    RootLink -> "/"
    AboutLink -> "/about"

instance FromJSON SiteLinks where
  parseJSON json = case json of
    String s
      | s == "/" -> pure RootLink
      | s == "/about" -> pure AboutLink
      | otherwise -> fail
    _ -> fail
    where
      fail = typeMismatch "SiteLinks" json

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
