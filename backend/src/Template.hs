{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , NamedFieldPuns
  , QuasiQuotes
  , StandaloneDeriving
  #-}

module Template where

import           Types (AppM)
import           Types.Env (Env (..), Development (..))
import           Types.Keys (Keys (..))
import           Links (WebAssetLinks (..))

import           Lucid (renderBST, HtmlT, Attribute, content_, name_, meta_, httpEquiv_, charset_, link_, rel_, type_, href_, sizes_)
import           Lucid.Base (makeAttribute)
import           Network.HTTP.Types (Status, status200)
import qualified Network.Wai.Middleware.ContentType.Types as CT
import           Web.Page.Lucid (template, WebPage (..))
import           Web.Routes.Nested (FileExtListenerT, mapHeaders, mapStatus, bytestring)

import qualified Data.Text                                as T
import qualified Data.Text.Encoding                       as T
import           Data.Default
import qualified Data.HashMap.Strict                      as HM
import           Data.Markup                              as M
import           Data.Url (AbsoluteUrlT (..), packLocation)
import           Data.URI (URI (..))
import           Data.URI.Auth (URIAuth (..))
import           Data.URI.Auth.Host (URIAuthHost (..))
import           Data.Aeson (ToJSON (..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as LBS
import qualified Data.Strict.Maybe                        as Strict
import           Data.Monoid ((<>))
import           Text.Heredoc (here)
import           Control.Monad.Trans                      (lift)
import           Control.Monad.Reader                     (ask)
import           Control.Monad.State                      (modify)
import           Control.Monad.Trans                      (lift)
import           Control.Monad.Morph                      (hoist)
import           Path.Extended (ToLocation (toLocation))

import Debug.Trace (traceShow)


deriving instance Show URIAuthHost
deriving instance Show URIAuth
deriving instance Show URI


htmlLight :: Status
          -> HtmlT (AbsoluteUrlT AppM) a
          -> FileExtListenerT AppM ()
htmlLight s content = do
  bs <- lift $ do
    Env{envHostname,envTls} <- ask
    let locationToURI loc =
          let uri = packLocation (Strict.Just $ if envTls then "https" else "http") True envHostname loc
          in  traceShow uri uri
    runAbsoluteUrlT (renderBST content) locationToURI

  bytestring CT.Html bs
  modify . HM.map $ mapStatus (const s)
                  . mapHeaders ([("content-Type", "text/html")] ++)


html :: HtmlT (AbsoluteUrlT AppM) ()
     -> FileExtListenerT AppM ()
html content = htmlLight status200 $ mainTemplate content


masterPage :: WebPage (HtmlT (AbsoluteUrlT AppM) ()) T.Text [Attribute]
masterPage =
  let page :: WebPage (HtmlT (AbsoluteUrlT AppM) ()) T.Text [Attribute]
      page = def
  in  page
        { metaVars = do
            link_ [href_ "https://fonts.googleapis.com/css?family=Roboto:300,400,500", rel_ "stylesheet"]
            link_ [href_ "https://cdnjs.cloudflare.com/ajax/libs/flag-icon-css/2.8.0/css/flag-icon.min.css", rel_ "stylesheet"]
            meta_ [charset_ "utf-8"]
            meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge,chrome=1"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, maximum-scale=1.0"]
            link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/apple-touch-icon.png"]
            link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/favicon-32x32.png"]
            link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/favicon-16x16.png"]
            link_ [rel_ "manifest", href_ "/site.webmanifest"]
            link_ [rel_ "mask-icon", href_ "/safari-pinned-tab.svg", makeAttribute "color" "#c62828"]
            meta_ [name_ "msapplication-TileColor", content_ "#c62828"]
            meta_ [name_ "theme-color", content_ "#ffffff"]
        , pageTitle = "Local Cooking"
        , styles =
          inlineStyles
        , bodyScripts = do
          Env{envDevelopment = mDev} <- lift ask
          deploy M.JavaScript M.Remote =<< lift (toLocation $ IndexJs $ devCacheBuster <$> mDev)
        }
  where
    inlineStyles =
      deploy M.Css Inline ([here|
a:link:not(.MuiButton-root-38), a:active:not(.MuiButton-root-38) {
  color: #c62828;
}
a:hover:not(.MuiButton-root-38) {
  color: #ff5f52;
}
a:visited:not(.MuiButton-root-38) {
  color: #8e0000;
}
body {
  background-color: #8e0000 !important;
  padding-bottom: 5em;
}|] :: T.Text)

-- | Inject some HTML into the @<body>@ tag of our template
mainTemplate :: HtmlT (AbsoluteUrlT AppM) ()
             -> HtmlT (AbsoluteUrlT AppM) ()
mainTemplate = template masterPage

