{-# LANGUAGE
    MultiParamTypeClasses
  , RecordWildCards
  , OverloadedStrings
  , OverloadedLists
  , DeriveGeneric
  , QuasiQuotes
  #-}

module Links where

import Data.Monoid ((<>))
import Path.Extended (ToPath (..), ToLocation (..), Abs, File, fromPath, setFileExt, addQuery, parseAbsFile, parseAbsDir)
import Path (absdir, absfile)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.ByteString.Base16 as BS16
import Crypto.Saltine.Core.Box (Nonce)
import qualified Crypto.Saltine.Class as NaCl
import Unsafe.Coerce (unsafeCoerce)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), oneof)


data WebAssetLinks
  = IndexCss -- FIXME Cache buster
  | IndexJs (Maybe Nonce)

instance ToPath WebAssetLinks Abs File where
  toPath x = case x of
    IndexCss  -> [absfile|/index|]
    IndexJs _ -> [absfile|/index|]

instance ToLocation WebAssetLinks Abs File where
  toLocation x = case x of
    IndexCss -> setFileExt (Just "css") $ fromPath $ toPath x
    IndexJs mNonce ->
        setFileExt (Just "js")
      $ ( case mNonce of
            Nothing -> id
            Just nonce -> addQuery ("cache_buster", Just $ BS8.toString $ BS16.encode $ NaCl.encode nonce)
        )
      $ fromPath
      $ toPath x


data SiteLinks
  = RootLink
  | AboutLink
  | MealsLink
  | ChefsLink
  | RegisterLink
  deriving (Eq, Show, Generic)

instance Arbitrary SiteLinks where
  arbitrary = oneof
    [ pure RootLink
    , pure AboutLink
    , pure MealsLink
    , pure ChefsLink
    , pure RegisterLink
    ]

-- TODO URI / Location parser

instance ToJSON SiteLinks where
  toJSON = String . T.pack . show . toLocation

instance FromJSON SiteLinks where
  parseJSON json = case json of
    String s
      | s == "/" -> pure RootLink
      | s == "/about" -> pure AboutLink
      | s == "/meals" -> pure MealsLink
      | s == "/chefs" -> pure ChefsLink
      | s == "/register" -> pure RegisterLink
      | otherwise -> fail
    _ -> fail
    where
      fail = typeMismatch "SiteLinks" json

instance ToPath SiteLinks Abs File where
  toPath x = case x of
    RootLink -> unsafeCoerce [absdir|/|]
    AboutLink -> [absfile|/about|]
    MealsLink -> [absfile|/meals|]
    ChefsLink -> [absfile|/chefs|]
    RegisterLink -> [absfile|/register|]

instance ToLocation SiteLinks Abs File where
  toLocation = fromPath . toPath


data LogoLinks
  = LogoPng
  | LogoWhitePng
  | IconPng
  | IconSvg


instance ToPath LogoLinks Abs File where
  toPath x = case x of
    LogoPng      -> [absfile|/static/images/logo|]
    LogoWhitePng -> [absfile|/static/images/logo-white|]
    IconPng      -> [absfile|/static/images/icon|]
    IconSvg      -> [absfile|/static/images/icon|]


instance ToLocation LogoLinks Abs File where
  toLocation x = case x of
    LogoPng      -> setFileExt (Just "png") $ fromPath $ toPath x
    LogoWhitePng -> setFileExt (Just "png") $ fromPath $ toPath x
    IconPng      -> setFileExt (Just "png") $ fromPath $ toPath x
    IconSvg      -> setFileExt (Just "svg") $ fromPath $ toPath x
