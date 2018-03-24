{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , ScopedTypeVariables
  , RecordWildCards
  , DataKinds
  #-}

module Server.HTTP where

import Server.HTTP.WebSocket (websocket)
import Server.Assets (favicons, frontend)
import Types (AppM, runAppM, HTTPException (..), LoginException (..))
import Types.Env (Env (..), Database (..), Managers (..), isDevelopment, Development (..))
import Types.FrontendEnv (FrontendEnv (..))
import Types.Keys (Keys (..))
import Template (html)
import Database (User (..), Users (..), Username (..), Email (..), Salt (..), InsertUser (..), GetUserSalt (..), GetAllUsers (..), IsUniqueSalt (..), salt, IsUniqueUsername (..))
import LocalCooking.Auth (UserID, SessionID, sessionID, ChallengeID (..), SignedChallenge, verifySignedChallenge)
import LocalCooking.WebSocket (LocalCookingInput (..), LocalCookingOutput (..), LocalCookingLoginResult (..))
import Links (FacebookLoginVerify (..), facebookLoginVerifyToURI)
import Login (ThirdPartyLoginToken (..))
import Login.Facebook (FacebookLoginReturn (..), FacebookLoginGetToken (..))

import Web.Routes.Nested (RouterT, match, matchHere, matchGroup, action, post, get, json, text, l_, (</>), o_, route)
import Network.Wai (strictRequestBody, queryString)
import Network.Wai.Middleware.ContentType (bytestring, FileExt (Other, JavaScript))
import Network.Wai.Trans (MiddlewareT)
import Network.WebSockets (defaultConnectionOptions)
import Network.WebSockets.Trans (websocketsOrT)
import Network.HTTP.Client (httpLbs, responseBody, parseRequest)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy   as LBS
import Data.URI (URI (..), printURI)
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson.Types (typeMismatch, Value (String, Object))
import qualified Data.Aeson as Aeson
import Data.Acid.Advanced (update', query')
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TimeMap
import Data.TimeMap.Multi (TimeMultiMap)
import qualified Data.TimeMap.Multi as TimeMultiMap
import qualified Data.Attoparsec.Text as Atto
import qualified Data.IxSet as IxSet
import Data.Monoid ((<>))
import qualified Data.Strict.Maybe as Strict
import Control.Applicative ((<|>))
import Control.Monad (join, when, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Control.Exception.Safe (throwM)
import Control.Logging (log', warn')
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMapChan (TMapChan, newTMapChan)
import qualified Control.Concurrent.STM.TMapChan as TMapChan
import Control.Concurrent.Chan.Scope (Scope (..))
import Control.Concurrent.STM.TChan.Typed (TChanRW)
import Crypto.Saltine.Core.Box (newNonce)
import qualified Crypto.Saltine.Class as NaCl
import System.IO.Error (userError)



data RegisterSubmit = RegisterSubmit
  { registerSubmitUsername        :: Username
  , registerSubmitSalt            :: Salt
  , registerSubmitEmail           :: Email
  , registerSubmitUserID          :: UserID
  , registerSubmitSignedChallenge :: SignedChallenge
  }

instance FromJSON RegisterSubmit where
  parseJSON (Object o) =
    RegisterSubmit <$> o .: "username"
                   <*> o .: "salt"
                   <*> o .: "email"
                   <*> o .: "userID"
                   <*> o .: "signedChallenge"
  parseJSON x = typeMismatch "RegisterSubmit" x


router :: (TChanRW 'Write (SessionID, LocalCookingInput), TMapChan SessionID LocalCookingOutput)
       -> (TChanRW 'Write (UserID, LocalCookingInput), TMapChan UserID LocalCookingOutput)
       -> TimeMap ChallengeID SessionID
       -> TimeMultiMap UserID SessionID
       -> TMapChan SessionID UserID
       -> RouterT (MiddlewareT AppM) sec AppM ()
router
  unauth@(incomingUnauth, outgoingUnauth)
  auth@(incomingAuth, outgoingAuth)
  challenges
  loginSessions
  loginRefs
  = do
  matchHere $ action $ get $ html Nothing ""
  match (l_ "about" </> o_) $ action $ get $ html Nothing "" -- FIXME SEO

  forM_ favicons $ \(file, content) -> do
    let (file', ext) = T.breakOn "." (T.pack file)
    match (l_ file' </> o_) $ action $ get $
      bytestring (Other (T.dropWhile (== '.') ext)) (LBS.fromStrict content)

  match (l_ "index.js" </> o_) $ \app req resp -> do
    Env{envDevelopment} <- ask
    case envDevelopment of
      Nothing -> pure ()
      Just Development{devCacheBuster} -> case join $ lookup "cache_buster" $ queryString req of
        Nothing -> fail "No cache busting parameter!"
        Just cacheBuster
          | cacheBuster == BS16.encode (NaCl.encode devCacheBuster) -> pure ()
          | otherwise -> fail "Wrong cache buster!" -- FIXME make cache buster generic
    (action $ get $ bytestring JavaScript $ LBS.fromStrict frontend) app req resp

  match (l_ "register" </> o_) $ \app req respond -> do
    body <- liftIO $ strictRequestBody req
    (u,sid) <- liftIO $ case Aeson.decode body of
      Nothing -> throwM $ JSONDecodingFailed body
      Just RegisterSubmit{..} -> case verifySignedChallenge registerSubmitUserID registerSubmitSignedChallenge of
        Nothing -> throwM $ ChallengeResponseInvalidSignature registerSubmitUserID registerSubmitSignedChallenge
        Just challenge -> do
          r <- liftIO $ atomically $ TimeMap.lookup challenge challenges
          case r of
            Nothing -> throwM $ ChallengeDoesntExist challenge
            Just sid' -> do
              liftIO $ atomically $ TimeMap.delete challenge challenges
              pure
                ( User
                  { userUsername = registerSubmitUsername
                  , userSalt = registerSubmitSalt
                  , userEmail = registerSubmitEmail
                  , userID = registerSubmitUserID
                  }
                , sid'
                )
    -- FIXME request for email confirmation
    Env{envDatabase = Database{dbUsers}} <- ask
    mR <- update' dbUsers $ InsertUser u
    case mR of
      Just e -> error (show e)
      Nothing -> do
        liftIO $ do
          putStr "Added user: "
          print u
        liftIO $ do
          TimeMultiMap.insert (userID u) sid loginSessions
          atomically $ TMapChan.insert outgoingAuth (userID u) $ LocalCookingLoginResult $ LocalCookingLoginSuccess u
          atomically $ TMapChan.insert loginRefs sid (userID u)
        let resp = action $ post $ \_ -> json $ String "success"
        resp app req respond

  match (l_ "userSalt" </> o_) $ \app req respond -> do
    username <- case join (lookup "username" (queryString req)) of
      Nothing -> throwM NoUsername
      Just x -> pure $ T.decodeUtf8 x
    Env{envDatabase = Database{dbUsers}} <- ask
    liftIO $ do
      putStr "Looking up user: "
      print username
    s <- query' dbUsers $ GetUserSalt (Username username)
    let resp = action $ get $ json s
    resp app req respond

  match (l_ "isUniqueSalt" </> o_) $ \app req respond -> do
    salt' <- case join (lookup "salt" (queryString req)) of
      Nothing -> throwM NoSalt
      Just s -> case Atto.parseOnly salt (T.decodeUtf8 s) of
        Left e -> throwM (SaltParseError e)
        Right x -> pure x
    Env{envDatabase = Database{dbUsers}} <- ask
    r <- query' dbUsers $ IsUniqueSalt salt'
    let resp = action $ get $ json r
    resp app req respond

  match (l_ "isUniqueUsername" </> o_) $ \app req respond -> do
    username <- case join (lookup "username" (queryString req)) of
      Nothing -> throwM NoUsername
      Just s -> pure (Username (T.decodeUtf8 s))
    Env{envDatabase = Database{dbUsers}} <- ask
    r <- query' dbUsers $ IsUniqueUsername username
    let resp = action $ get $ json r
    resp app req respond

  match (l_ "challenge" </> o_) $ \app req respond ->
    case join (lookup "sessionID" (queryString req)) of
      Nothing -> fail "No session id"
      Just sid' -> case Atto.parseOnly sessionID (T.decodeUtf8 sid') of
        Left e -> fail $ "SessionID parsing failed: " ++ e
        Right sid -> do
          challenge <- ChallengeID <$> liftIO newNonce
          liftIO $ TimeMap.insert challenge sid challenges
          let resp = action $ get $ json challenge
          resp app req respond

  env <- lift ask
  when (isDevelopment env) $
    match (l_ "getAllUsers" </> o_) $ \app req respond -> do
      Env{envDatabase = Database{dbUsers}} <- ask
      Users us <- query' dbUsers GetAllUsers
      let resp = action $ get $ json $ IxSet.toList us
      resp app req respond

  match (l_ "realtime" </> o_) $ \app req resp ->
    case join (lookup "sessionID" (queryString req)) of
      Nothing -> fail $ "No session id: " ++ show (queryString req)
      Just sid' -> case Atto.parseOnly sessionID (T.decodeUtf8 sid') of
        Left e -> fail $ "SessionID parsing failed: " ++ e
        Right sid -> do
          env <- ask
          (websocketsOrT defaultConnectionOptions
            (websocket sid unauth auth challenges loginSessions loginRefs)) app req resp
  -- match (l_ "confirmEmail" </> o_) $ \req ->
  --   let resp = action $ get $ text "yo"
  --   in  resp req


  match (l_ "facebookLoginReturn" </> o_) $ \app req resp ->
    case do let bad = do
                  errorCode <- join $ lookup "error_code" $ queryString req
                  errorMessage <- join $ lookup "error_message" $ queryString req
                  pure $ FacebookLoginReturnBad errorCode errorMessage
                denied = do
                  error' <- join $ lookup "error" $ queryString req
                  errorReason <- join $ lookup "error_reason" $ queryString req
                  errorDescription <- join $ lookup "error_description" $ queryString req
                  if error' == "access_denied" && errorReason == "user_denied"
                    then pure $ FacebookLoginReturnDenied errorDescription
                    else Nothing
                good = do
                  code <- fmap T.decodeUtf8 $ join $ lookup "code" $ queryString req
                  (state :: Maybe ()) <- do -- FIXME decide a monomorphic state to share for CSRF prevention
                    x <- join $ lookup "state" $ queryString req
                    Aeson.decode (LBS.fromStrict x)
                  pure $ FacebookLoginReturnGood code state
            bad <|> good <|> denied of
      Nothing -> fail $ "No parameters: " <> show (queryString req)
      Just x -> do
        case x of
          FacebookLoginReturnDenied{} -> do
            warn' $ "Got bad facebook login: " <> T.pack (show x) -- FIXME return 200, pack failure in frontendEnv
            throwM $ userError "Bad facebook login"
          FacebookLoginReturnBad{} -> do
            warn' $ "Got bad facebook login: " <> T.pack (show x) -- FIXME return 200, pack failure in frontendEnv
            throwM $ userError "Bad facebook login"
          FacebookLoginReturnGood{facebookLoginGoodCode} -> do
            Env
              { envManagers = Managers{managersFacebook}
              , envKeys = Keys{keysFacebookClientID, keysFacebookClientSecret}
              , envHostname
              } <- ask

            let url = printURI $ facebookLoginVerifyToURI FacebookLoginVerify
                  { facebookLoginVerifyClientID = keysFacebookClientID
                  , facebookLoginVerifyClientSecret = keysFacebookClientSecret
                  , facebookLoginVerifyRedirectURI = URI (Strict.Just "https") True envHostname ["facebookLoginReturn"] [] Strict.Nothing
                  , facebookLoginVerifyCode = facebookLoginGoodCode
                  }

            req' <- liftIO $ parseRequest (T.unpack url)
            resp' <- liftIO $ httpLbs req' managersFacebook

            case Aeson.decode (responseBody resp') of
              Nothing -> do
                throwM $ userError $ "Somehow couldn't parse facebook verify output: " <> show (responseBody resp')
              Just x -> do
                case x of
                  FacebookLoginGetTokenError{} -> do
                    when (isDevelopment env) $ warn' $
                      "Couldn't verify facebook code due to formatting error: "
                      <> T.pack (show x) <> ", from: " <> url
                    throwM $ userError "Couldn't verify facebook code" -- FIXME
                  FacebookLoginGetToken{facebookLoginGetTokenAccessToken} -> do
                    log' $ "Got facebook access token: " <> T.pack (show x)
                    (action $ get $ html (Just $ FacebookLoginToken facebookLoginGetTokenAccessToken) "") app req resp

  match (l_ "facebookLoginDeauthorize" </> o_) $ \app req resp -> do
    body <- liftIO $ strictRequestBody req
    log' $ "Got deauthorized: " <> T.pack (show body)
    (action $ post $ \_ -> text "") app req resp



httpServer :: (TChanRW 'Write (SessionID, LocalCookingInput), TMapChan SessionID LocalCookingOutput)
           -> (TChanRW 'Write (UserID, LocalCookingInput), TMapChan UserID LocalCookingOutput)
           -> TimeMap ChallengeID SessionID
           -> TimeMultiMap UserID SessionID
           -> RouterT (MiddlewareT AppM) sec AppM ()
           -> MiddlewareT AppM
httpServer unauth auth challenges loginSessions dependencies = \app req resp -> do
  loginRefs <- liftIO $ atomically newTMapChan
  route ( do dependencies
             router unauth auth challenges loginSessions loginRefs
        ) app req resp
