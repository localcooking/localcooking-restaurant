module Links where

import LocalCooking.Auth (SessionID)
import Env (env)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.List (List (..))
import Data.URI (URI (..), Scheme (..), HierarchicalPart (..), Query (..), Host (..), Authority (..))
import Data.URI.URI (print) as URI
import Data.URI.Location (Location (..))
import Data.Argonaut (encodeJson)
import Data.Path.Pathy (Path, Abs, File, Sandboxed, (</>), dir, file, rootDir, printPath)
import Data.Generic (class Generic, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.String (char, string)
import Control.Alternative ((<|>))
import DOM.HTML.History (DocumentTitle (..))
import Global (encodeURIComponent)


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

data WebSocketLinks
  = Realtime SessionID

instance toLocationWebSocketLinks :: ToLocation WebSocketLinks where
  toLocation (Realtime sessionID) =
    Location (Right $ rootDir </> file "realtime") (Just $ Query $ Cons (Tuple "sessionID" $ Just $ show sessionID) Nil) Nothing


data SiteLinks
  = RootLink
  | AboutLink
  | MealsLink -- FIXME search terms
  | ChefsLink -- FIXME search terms or hierarchy

derive instance genericSiteLinks :: Generic SiteLinks

instance showSiteLinks :: Show SiteLinks where
  show x = case x of
    RootLink -> printPath rootDir
    AboutLink -> printPath $ rootDir </> file "about"
    MealsLink -> printPath $ rootDir </> file "meals"
    ChefsLink -> printPath $ rootDir </> file "chefs"

instance eqSiteLinks :: Eq SiteLinks where
  eq = gEq

instance encodeJsonSiteLinks :: EncodeJson SiteLinks where
  encodeJson = gEncodeJson

instance decodeJsonSiteLinks :: DecodeJson SiteLinks where
  decodeJson = gDecodeJson

siteLinksToDocumentTitle :: SiteLinks -> DocumentTitle
siteLinksToDocumentTitle x = DocumentTitle $ case x of
  RootLink -> "Local Cooking"
  AboutLink -> "About - Local Cooking"
  MealsLink -> "Meals - Local Cooking"
  ChefsLink -> "Chefs - Local Cooking"

siteLinksParser :: Parser SiteLinks
siteLinksParser = do
  let root = RootLink <$ divider
      about = do
        void divider
        AboutLink <$ string "about"
      meals = do
        void divider
        MealsLink <$ string "meals" -- FIXME search parameters
      chefs = do
        void divider
        ChefsLink <$ string "chefs" -- FIXME search parameters or hierarchy
  try about
    <|> try meals
    <|> try chefs
    <|> root
  where
    divider = char '/'


data ThirdPartyLoginLinks a
  = FacebookLoginLink
    { redirectURL :: URI
    , state :: a
    }

thirdPartyLoginLinksToURI :: forall a. EncodeJson a => ThirdPartyLoginLinks a -> URI
thirdPartyLoginLinksToURI x = case x of
  FacebookLoginLink {redirectURL,state} ->
    URI
      (Just $ Scheme "https")
      ( HierarchicalPart
        (Just $ Authority Nothing [Tuple (NameAddress "www.facebook.com") Nothing])
        (Just $ Right $ rootDir </> dir "v2.12" </> dir "dialog" </> file "oauth")
      )
      ( Just $ Query
        $ Cons
          (Tuple "client_id" $ Just env.facebookClientID)
        $ Cons
          (Tuple "redirect_uri" $ Just $ URI.print redirectURL)
        $ Cons
          (Tuple "state" $ Just $ show $ encodeJson state)
          Nil
      )
      Nothing


data ThirdPartyLoginReturnLinks
  = FacebookLoginReturn

instance toLocationThirdPartyLoginReturnLinks :: ToLocation ThirdPartyLoginReturnLinks where
  toLocation x = case x of
    FacebookLoginReturn -> Location (Right $ rootDir </> file "facebookLoginReturn") Nothing Nothing
