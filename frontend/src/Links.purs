module Links where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI.Location (Location (..))
import Data.Path.Pathy (Path, Abs, File, Sandboxed, (</>), dir, file, rootDir, printPath)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (char, string)
import Control.Alternative ((<|>))
import DOM.HTML.History (DocumentTitle (..))


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
  | LoginLink

instance showSiteLinks :: Show SiteLinks where
  show x = case x of
    RootLink -> printPath rootDir
    AboutLink -> printPath $ rootDir </> file "about"
    LoginLink -> printPath $ rootDir </> file "login"


siteLinksToDocumentTitle :: SiteLinks -> DocumentTitle
siteLinksToDocumentTitle x = DocumentTitle $ case x of
  RootLink -> "Local Cooking"
  AboutLink -> "About - Local Cooking"
  LoginLink -> "Login - Local Cooking"

siteLinksParser :: Parser SiteLinks
siteLinksParser = do
  let root = RootLink <$ divider
      about = do
        void divider
        AboutLink <$ string "about"
      login = do
        void divider
        LoginLink <$ string "login"
  about
    <|> login
    <|> root
  where
    divider = char '/'

