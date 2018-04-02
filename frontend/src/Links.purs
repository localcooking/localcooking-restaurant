module Links where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (Query (..))
import Data.URI.URI as URI
import Data.URI.Path as URIPath
import Data.URI.Location (Location (..), fromURI, printLocation, parseLocation)
import Data.URI.Location as Location
import Data.StrMap as StrMap
import Data.Path.Pathy ((</>), dir, file, rootDir)
import Data.Generic (class Generic, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Data.NonEmpty ((:|))
import Data.Foreign (toForeign, unsafeFromForeign)
import Text.Parsing.StringParser (Parser, try, runParser)
import Text.Parsing.StringParser.String (char, string, eof)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, warn)
import Control.Monad.Eff.Uncurried (mkEffFn1, runEffFn2)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location, history)
import DOM.HTML.Window.Extra (onPopStateImpl)
import DOM.HTML.Location (href)
import DOM.HTML.History (DocumentTitle (..), pushState, replaceState, URL (..))
import DOM.HTML.Types (History, HISTORY, Window)
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
  | RegisterLink
  | MealsLink -- FIXME search terms
  | ChefsLink -- FIXME search terms or hierarchy

instance arbitrarySiteLinks :: Arbitrary SiteLinks where
  arbitrary = oneOf $
        (pure RootLink)
    :|  [ pure MealsLink
        , pure ChefsLink
        , pure RegisterLink
        ]

initSiteLinks :: forall eff
               . Eff ( console :: CONSOLE
                     , dom     :: DOM
                     , history :: HISTORY
                     | eff) SiteLinks
initSiteLinks = do
  w <- window
  l <- location w
  h <- history w
  p <- href l
  case URI.parse p of
    Left e -> do
      warn $ "Href parsing error: " <> show e
      replaceState' RootLink h
      pure RootLink
    Right uri -> case fromURI uri of
      Nothing -> do
        warn $ "URI can't be a location: " <> show uri
        replaceState' RootLink h
        pure RootLink
      Just {location: location@(Location _ mQuery _)} -> case siteLinksParser location of
        Nothing -> do
          warn $ "Location can't be a SiteLinks: " <> show location
          replaceState' RootLink h
          pure RootLink
        Just x -> do
          -- FIXME only adjust for authToken when it's parsable?
          case mQuery of
            Nothing -> pure unit
            Just (Query qs) -> case StrMap.lookup "authToken" (StrMap.fromFoldable qs) of
              Nothing -> pure unit
              Just _ -> replaceState' x h
          pure x

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
    MealsLink -> Location (Right $ rootDir </> file "meals") Nothing Nothing
    ChefsLink -> Location (Right $ rootDir </> file "chefs") Nothing Nothing
    RegisterLink -> Location (Right $ rootDir </> file "register") Nothing Nothing

siteLinksToDocumentTitle :: SiteLinks -> DocumentTitle
siteLinksToDocumentTitle x = DocumentTitle $ case x of
  RootLink -> "Local Cooking"
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
      try meals
        <|> try chefs
        <|> try register
        <|> root
      where
        divider = char '/'



pushState' :: forall eff. SiteLinks -> History -> Eff (history :: HISTORY | eff) Unit
pushState' x h = do
  pushState
    (toForeign $ encodeJson x)
    (siteLinksToDocumentTitle x)
    (URL $ Location.printLocation $ toLocation x)
    h


replaceState' :: forall eff. SiteLinks -> History -> Eff (history :: HISTORY | eff) Unit
replaceState' x h = do
  replaceState
    (toForeign $ encodeJson x)
    (siteLinksToDocumentTitle x)
    (URL $ Location.printLocation $ toLocation x)
    h


onPopState :: forall eff
            . (SiteLinks -> Eff (dom :: DOM, exception :: EXCEPTION | eff) Unit)
           -> Window
           -> Eff (dom :: DOM, exception :: EXCEPTION | eff) Unit
onPopState go w =
  onPopState' \fgn -> case decodeJson (unsafeFromForeign fgn) of
    Left e -> throw e
    Right (x :: SiteLinks) -> go x
  where
    onPopState' f = runEffFn2 onPopStateImpl (mkEffFn1 f) w


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
