{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , NamedFieldPuns
  #-}

module Template where

import           Types (AppM)
import           Types.Env (Env (..))
import           Server.Assets (frontend)

import           Lucid (renderBST, HtmlT, Attribute, content_, name_, meta_, httpEquiv_, charset_)
import           Network.HTTP.Types (Status, status200)
import qualified Network.Wai.Middleware.ContentType.Types as CT
import           Web.Page.Lucid (template, WebPage (..))
import           Web.Routes.Nested (FileExtListenerT, mapHeaders, mapStatus, bytestring)

import qualified Data.Text                                as T
import qualified Data.Text.Encoding                       as T
import           Data.Default
import qualified Data.HashMap.Strict                      as HM
import           Data.Markup                              as M
import           Data.Url (AbsoluteUrlT (..), fromLocation)
import           Data.Aeson (ToJSON (..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy                     as LBS
import           Data.Monoid ((<>))
import           Control.Monad.Reader                     (ask)
import           Control.Monad.State                      (modify)
import           Control.Monad.Trans                      (lift)



htmlLight :: Status
          -> HtmlT (AbsoluteUrlT AppM) a
          -> FileExtListenerT AppM ()
htmlLight s content = do
  hostname <- envHostname <$> lift ask
  bs <- lift $ runAbsoluteUrlT (renderBST content) (fromLocation (Just "http") True hostname)

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
  in  page { metaVars = do
               meta_ [charset_ "utf-8"]
               meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge,chrome=1"]
               meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, maximum-scale=1.0"]
           , pageTitle = "Local Cooking"
           , styles =
               inlineStyles
           , bodyScripts =
               inlineBodyScripts
           }
  where
    inlineStyles =
      deploy M.Css Inline ("body {background: #aaa;}" :: T.Text)

    inlineBodyScripts = do
      Env{envDevelopment} <- lift ask
      deploy M.JavaScript Inline $
        "var frontendEnv = "
        <> T.decodeUtf8 (LBS.toStrict $ Aeson.encode FrontendEnv{frontendEnvDevelopment = envDevelopment})
      deploy M.JavaScript Inline $ T.decodeUtf8 frontend

-- | Inject some HTML into the @<body>@ tag of our template
mainTemplate :: HtmlT (AbsoluteUrlT AppM) ()
             -> HtmlT (AbsoluteUrlT AppM) ()
mainTemplate = template masterPage



data FrontendEnv = FrontendEnv
  { frontendEnvDevelopment :: Bool
  }

instance ToJSON FrontendEnv where
  toJSON FrontendEnv{frontendEnvDevelopment} = object
    [ "development" .= frontendEnvDevelopment
    ]
