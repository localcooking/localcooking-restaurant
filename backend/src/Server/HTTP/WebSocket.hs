{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  #-}

module Server.HTTP.WebSocket where

import Server.Subs (delSession)
import Data.TimeMap.Multi (TimeMultiMap)
import qualified Data.TimeMap.Multi as TimeMultiMap
import Types (AppM, LoginException (..))
import Types.Env (Env (..), Database (..))
import Database (GetUserDetails (..))
import LocalCooking.Auth (UserID, SessionID, ChallengeID (..), verifySignedChallenge)
import LocalCooking.WebSocket (LocalCookingInput (..), LocalCookingOutput (..), LocalCookingLoginResult (..))

import Network.Wai.Trans (ServerAppT)
import Network.WebSockets (DataMessage (..), sendTextData, receiveDataMessage, DataMessage (..), acceptRequest, ConnectionException (..))
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Aeson as Aeson
import Data.Acid.Advanced (query')
import Data.Singleton.Class (runSingleton)
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TimeMap
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Control.Aligned (liftBaseWith)
import Control.Exception.Safe (throwM, catch)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMapChan (TMapChan)
import Control.Concurrent.STM.TMVar (TMVar, readTMVar, newEmptyTMVar, putTMVar, tryReadTMVar, tryTakeTMVar)
import qualified Control.Concurrent.STM.TMapChan as TMapChan
import Control.Concurrent.Chan.Scope (Scope (..))
import Control.Concurrent.STM.TChan.Typed (TChanRW, writeTChanRW)
import Control.Logging (log', debug')



websocket :: SessionID
          -> (TChanRW 'Write (SessionID, LocalCookingInput), TMapChan SessionID LocalCookingOutput)
          -> (TChanRW 'Write (UserID, LocalCookingInput), TMapChan UserID LocalCookingOutput)
          -> TimeMap ChallengeID SessionID
          -> TimeMultiMap UserID SessionID
          -> TMapChan SessionID UserID
          -> ServerAppT AppM
websocket
  sid
  (incomingUnauth, outgoingUnauth)
  (incomingAuth, outgoingAuth)
  challenges
  loginSessions
  loginRefs
  = \pending -> do

  (userIDRef :: TMVar UserID) <- liftIO $ atomically newEmptyTMVar
  Env{envDevelopment} <- ask

  let go = do
        conn <- liftIO $ acceptRequest pending

        when envDevelopment $ log' $ "Accepted connection from: " <> T.pack (show sid)

        void $ liftBaseWith $ \runInBase -> do
          void $ async $ forever $ do
            msg <- receiveDataMessage conn
            let msg' = case msg of
                  Text bits _ -> bits
                  Binary bits -> bits
            case Aeson.eitherDecode msg' of
              Left e -> when envDevelopment $ debug' $ "Couldn't decode LocalCooking input: " <> T.pack (show (LT.decodeUtf8 msg')) <> T.pack e
              Right (x :: LocalCookingInput) -> do
                when envDevelopment $ log' $ "Got koredex input: " <> T.pack (show x)
                mUser <- atomically $ tryReadTMVar userIDRef
                case mUser of
                  Nothing ->
                    case x of
                      LocalCookingPing ->
                        -- instantly reply to ping messages without considering them in business logic
                        liftIO $ sendTextData conn $ T.decodeUtf8 $ LBS.toStrict $ Aeson.encode LocalCookingPong
                      LocalCookingLogin{koredexLoginUserID,koredexLoginSignedChallenge} -> do
                        case verifySignedChallenge koredexLoginUserID koredexLoginSignedChallenge of
                          Nothing        -> throwM $ ChallengeResponseInvalidSignature koredexLoginUserID koredexLoginSignedChallenge
                          Just challenge -> do
                            r <- atomically $ TimeMap.lookup challenge challenges
                            case r of
                              Nothing         -> throwM $ ChallengeDoesntExist challenge
                              Just sid'
                                | sid' /= sid -> throwM $ ChallengeForWrongSessionID sid' sid challenge
                                | otherwise   -> atomically $ TimeMap.delete challenge challenges
                        Env{envDatabase = Database{dbUsers}} <- runSingleton <$> runInBase ask
                        mu <- query' dbUsers $ GetUserDetails koredexLoginUserID
                        case mu of
                          Nothing -> throwM $ UserDoesntExist koredexLoginUserID
                          Just u -> do
                            atomically $ do
                              _ <- tryTakeTMVar userIDRef
                              putTMVar userIDRef koredexLoginUserID
                            TimeMultiMap.insert koredexLoginUserID sid loginSessions
                            atomically $ TMapChan.insert outgoingAuth koredexLoginUserID (LocalCookingLoginResult (LocalCookingLoginSuccess u))
                      _  ->
                        atomically $ writeTChanRW incomingUnauth (sid,x)
                  Just u -> atomically $ writeTChanRW incomingAuth (u,x)

          void $ async $ forever $ do
            userID <- atomically $ TMapChan.lookup loginRefs sid
            atomically $ do
              _ <- tryReadTMVar userIDRef
              putTMVar userIDRef userID

          void $ async $ forever $ do
            outgoing <- atomically $ TMapChan.lookup outgoingUnauth sid
            sendTextData conn $ T.decodeUtf8 $ LBS.toStrict $ Aeson.encode outgoing

          forever $ do
            userID <- atomically $ readTMVar userIDRef
            outgoing <- atomically $ TMapChan.lookup outgoingAuth userID
            sendTextData conn $ T.decodeUtf8 $ LBS.toStrict $ Aeson.encode outgoing

      handle :: ConnectionException -> AppM ()
      handle _ = do
        liftIO $ atomically $ TimeMultiMap.filter (\_ sid' -> sid' /= sid) loginSessions
        delSession sid

  go `catch` handle
