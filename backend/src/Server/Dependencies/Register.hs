{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , RecordWildCards
  #-}

module Server.Dependencies.Register where

import Types (AppM)
import Types.Env (Env (..), Managers (..))
import Types.Keys (Keys (..))
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Database.Query.User (registerUser, RegisterFailure)
import Google.ReCaptcha (ReCaptchaResponse, ReCaptchaVerifyResponse (..), googleReCaptchaVerifyURI)
import Google.Keys (GoogleCredentials (..))

import Text.EmailAddress (EmailAddress)
import qualified Text.EmailAddress as EmailAddress
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), (.:), Value (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import Data.URI (printURI)
import Data.URI.Auth.Host (printURIAuthHost)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Control.Logging (log')
import Network.HTTP.Client (httpLbs, responseBody, parseRequest, method, urlEncodedBody, RequestBody (RequestBodyLBS))
import Network.Mail.SMTP (sendMail, simpleMail, htmlPart, Address (..))

import Web.Dependencies.Sparrow (Server, ServerContinue (..), ServerReturn (..))
import Lucid (renderTextT)
import qualified Lucid.Html5 as L


data RegisterInitIn = RegisterInitIn
  { registerInitInEmail :: EmailAddress
  , registerInitInPassword :: HashedPassword
  , registerInitInReCaptcha :: ReCaptchaResponse
  }


instance FromJSON RegisterInitIn where
  parseJSON json = case json of
    Object o -> RegisterInitIn <$> o .: "email"
                               <*> o .: "password"
                               <*> o .: "reCaptcha"
    _ -> fail
    where fail = typeMismatch "RegisterInitIn" json



data RegisterInitOut
  = RegisterInitOutEmailSent
  | RegisterInitOutBadCaptcha
  | RegisterInitOutDBError RegisterFailure


instance ToJSON RegisterInitOut where
  toJSON x = case x of
    RegisterInitOutEmailSent -> String "email-sent"
    RegisterInitOutBadCaptcha -> String "bad-captcha"
    RegisterInitOutDBError e -> object ["db" .= e]



data RegisterDeltaIn


instance FromJSON RegisterDeltaIn where
  parseJSON = typeMismatch "RegisterDeltaIn"


data RegisterDeltaOut


instance ToJSON RegisterDeltaOut where
  toJSON _ = String ""



registerServer :: Server AppM RegisterInitIn
                              RegisterInitOut
                              RegisterDeltaIn
                              RegisterDeltaOut
registerServer RegisterInitIn{..} = do
  liftIO $ log' "running..."

  Env
    { envManagers = Managers{managersReCaptcha}
    , envKeys = Keys{keysGoogle = GoogleCredentials{googleReCaptchaSecret}}
    , envDatabase
    , envSMTPHost
    } <- ask

  liftIO $ do
    req <- parseRequest $ T.unpack $ printURI googleReCaptchaVerifyURI

    let -- body = Aeson.encode $ ReCaptchaVerify googleReCaptchaSecret registerInitInReCaptcha
        req' = urlEncodedBody
                 [ ("secret", LBS.toStrict $ Aeson.encode googleReCaptchaSecret)
                 , ("response", LBS.toStrict $ Aeson.encode registerInitInReCaptcha)
                 ]
                 $ req
                    { method = "POST"
                    }

    log' "sending..."

    resp <- httpLbs req' managersReCaptcha

    case Aeson.decode (responseBody resp) of
      Nothing -> do
        liftIO $ log' $ "Couldn't parse response body: " <> T.pack (show $ responseBody resp)
        pure Nothing
      Just (ReCaptchaVerifyResponse success)
        | not success -> do
          liftIO $ log' $ "recaptcha failure: " <> T.pack (show $ responseBody resp)
          pure $ Just ServerContinue
            { serverOnUnsubscribe = pure ()
            , serverContinue = \_ -> pure ServerReturn
              { serverInitOut = RegisterInitOutBadCaptcha
              , serverOnOpen = \_ -> pure Nothing
              , serverOnReceive = \_ _ -> pure ()
              }
            }
        | otherwise -> do
            eUid <- liftIO $ registerUser envDatabase registerInitInEmail registerInitInPassword
            case eUid of
              Left e -> do
                liftIO $ log' "db error"
                pure $ Just ServerContinue
                  { serverOnUnsubscribe = pure ()
                  , serverContinue = \_ -> pure ServerReturn
                    { serverInitOut = RegisterInitOutDBError e
                    , serverOnOpen = \_ -> pure Nothing
                    , serverOnReceive = \_ _ -> pure ()
                    }
                  }
              Right uid -> do
                liftIO $ log' "sending email..."
                -- Send registration email
                emailContent <- renderTextT $ do
                  L.div_ [] $ do
                    L.h1_ [] "Complete your Registration"
                    L.p_ [] "test"
                liftIO $ do
                  let message = simpleMail
                        (Address (Just "Local Cooking Webmaster") "noreply@localcooking.com")
                        [Address Nothing (EmailAddress.toText registerInitInEmail)]
                        []
                        []
                        "Complete your Registration with Local Cooking"
                        [htmlPart emailContent]
                  sendMail (T.unpack (printURIAuthHost envSMTPHost)) message
                liftIO $ log' "email sent."
                pure $ Just ServerContinue
                  { serverOnUnsubscribe = pure ()
                  , serverContinue = \_ -> pure ServerReturn
                    { serverInitOut = RegisterInitOutEmailSent
                    , serverOnOpen = \_ -> pure Nothing
                    , serverOnReceive = \_ _ -> pure ()
                    }
                  }
