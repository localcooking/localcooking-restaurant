module Links where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI.Location (Location (..), printLocation, parseLocation)
import Data.URI.Path as URIPath
import Data.Path.Pathy ((</>), dir, file, rootDir)
import Data.Generic (class Generic, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Data.NonEmpty ((:|))
import Text.Parsing.StringParser (Parser, try, runParser)
import Text.Parsing.StringParser.String (char, string, eof)
import Control.Alternative ((<|>))
import DOM.HTML.History (DocumentTitle (..))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (oneOf)


class ToLocation sym where
  toLocation :: sym -> Location


data LogoLinks
  = LogoPng
  | Logo40Png
  | LogoWhitePng
  | LogoWhite40Png
  | IconPng
  | IconSvg


instance toLocationLogoLinks :: ToLocation LogoLinks where
  toLocation x = case x of
    LogoPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo.png") Nothing Nothing
    Logo40Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-40.png") Nothing Nothing
    LogoWhitePng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-white.png") Nothing Nothing
    LogoWhite40Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-white-40.png") Nothing Nothing
    IconPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.png") Nothing Nothing
    IconSvg -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.svg") Nothing Nothing



data SiteLinks
  = RootLink
  | AboutLink
  | RegisterLink
  | MealsLink -- FIXME search terms
  | ChefsLink -- FIXME search terms or hierarchy

instance arbitrarySiteLinks :: Arbitrary SiteLinks where
  arbitrary = oneOf $
        (pure RootLink)
    :|  [ pure AboutLink
        , pure MealsLink
        , pure ChefsLink
        , pure RegisterLink
        ]

initSiteLinks :: SiteLinks
initSiteLinks = RootLink

derive instance genericSiteLinks :: Generic SiteLinks

instance showSiteLinks :: Show SiteLinks where
  show = printLocation <<< toLocation

instance eqSiteLinks :: Eq SiteLinks where
  eq = gEq

instance encodeJsonSiteLinks :: EncodeJson SiteLinks where
  encodeJson x = encodeJson (show x)

instance decodeJsonSiteLinks :: DecodeJson SiteLinks where
  decodeJson json = do
    s <- decodeJson json -- FIXME use location parser
    case runParser parseLocation s of
      Left e -> fail (show e)
      Right loc -> case siteLinksParser loc of
        Nothing -> fail "siteLinksParser failed"
        Just x -> pure x

instance toLocationSiteLinks :: ToLocation SiteLinks where
  toLocation x = case x of
    RootLink  -> Location (Left rootDir) Nothing Nothing
    AboutLink -> Location (Right $ rootDir </> file "about") Nothing Nothing
    MealsLink -> Location (Right $ rootDir </> file "meals") Nothing Nothing
    ChefsLink -> Location (Right $ rootDir </> file "chefs") Nothing Nothing
    RegisterLink -> Location (Right $ rootDir </> file "register") Nothing Nothing

siteLinksToDocumentTitle :: SiteLinks -> DocumentTitle
siteLinksToDocumentTitle x = DocumentTitle $ case x of
  RootLink -> "Local Cooking"
  AboutLink -> "About - Local Cooking"
  MealsLink -> "Meals - Local Cooking"
  ChefsLink -> "Chefs - Local Cooking"
  RegisterLink -> "Register - Local Cooking"


-- Policy: don't fail on bad query params / fragment unless you have to
siteLinksParser :: Location -> Maybe SiteLinks
siteLinksParser (Location path mQuery mFrag) = do
  case runParser siteLinksPathParser (URIPath.printPath path) of
    Left _ -> Nothing
    Right link -> pure link
  where
    siteLinksPathParser :: Parser SiteLinks
    siteLinksPathParser = do
      let root = RootLink <$ (divider *> eof)
          about = do
            void divider
            void (string "about")
            pure AboutLink
          meals = do
            void divider
            void (string "meals")
            pure MealsLink
          chefs = do
            void divider
            void (string "chefs")
            pure ChefsLink -- FIXME search parameters or hierarchy
          register = do
            void divider
            void (string "register")
            pure RegisterLink
      try about
        <|> try meals
        <|> try chefs
        <|> try register
        <|> root
      where
        divider = char '/'


data ThirdPartyLoginReturnLinks
  = FacebookLoginReturn -- (Maybe {code :: String, state :: Maybe Unit}) -- FIXME hardcode a facebook login state

instance toLocationThirdPartyLoginReturnLinks :: ToLocation ThirdPartyLoginReturnLinks where
  toLocation x = case x of
    FacebookLoginReturn -> Location (Right $ rootDir </> file "facebookLoginReturn") Nothing Nothing


thirdPartyLoginReturnLinksParser :: Parser ThirdPartyLoginReturnLinks
thirdPartyLoginReturnLinksParser = do
  let facebook = do
        void divider
        FacebookLoginReturn <$ (string "facebookLoginReturn" *> eof)
  facebook
  where
    divider = char '/'
