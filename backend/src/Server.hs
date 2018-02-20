{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , QuasiQuotes
  , DataKinds
  #-}

module Server where

import Server.HTTP (httpServer)
import Control (control, ControlSenders (..), ControlReceivers (..))
import Types (AppM)
import Types.Env (Env (..))
import LocalCooking.Auth (UserID, SessionID, ChallengeID (..))
import LocalCooking.WebSocket (LocalCookingInput (..), LocalCookingOutput (..))

import Web.Routes.Nested (textOnly)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Trans (ApplicationT, runApplicationT, runClientAppT)
import Network.WebSockets (ConnectionException (..), runClient, HandshakeException)
import Network.HTTP.Types (status404)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Singleton.Class (runSingleton)
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TimeMap
import Data.TimeMap.Multi (TimeMultiMap)
import qualified Data.TimeMap.Multi as TimeMultiMap
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Arrow ((***))
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Control.Aligned (liftBaseWith)
import Control.Exception.Safe (SomeException, throwM, catch)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMapChan (TMapChan, newTMapChan)
import qualified Control.Concurrent.STM.TMapChan as TMapChan
import Control.Concurrent.Chan.Scope (Scope (..))
import Control.Concurrent.STM.TChan.Typed (TChanRW, newTChanRW, writeTChanRW, readTChanRW, readOnly, writeOnly, allowReading)
import Control.Concurrent.STM.TChan.Typed.Extra (throttleStatic, intersperseStatic)
import Control.Logging (log', warn')
import Path.Extended (Location (..))
import Path (absfile)




server :: Int -> AppM ()
server port = do
  Env{envDevelopment} <- ask

  (loginSessions :: TimeMultiMap UserID SessionID) <- liftIO (atomically TimeMultiMap.newTimeMultiMap)
  (outgoingUnauth :: TMapChan SessionID LocalCookingOutput) <- liftIO (atomically newTMapChan)
  (outgoingAuth :: TMapChan UserID LocalCookingOutput) <- liftIO (atomically newTMapChan)
  (incomingUnauth :: TChanRW 'ReadWrite (SessionID, LocalCookingInput)) <- liftIO (atomically newTChanRW)
  (incomingAuth :: TChanRW 'ReadWrite (UserID, LocalCookingInput)) <- liftIO (atomically newTChanRW)
  (challenges :: TimeMap ChallengeID SessionID) <- liftIO $ do
    x <- atomically TimeMap.newTimeMap
    _ <- async $ forever $ do
      TimeMap.filterFromNow (60 * 5) x
      threadDelay (10 * 1000000)
    pure x


  -- -- BitMex Client
  -- void $ liftBaseWith $ \runInBase -> async $ do
  --   r <- runSingleton <$> runInBase genAuthRequest

  --   let bitmexURI = show $
  --         Location
  --           0
  --           [absfile|/realtime|]
  --           Nothing
  --           (map (T.unpack *** fmap T.unpack) (authRequestToQueryString r))
  --           Nothing

  --   when envDevelopment $ log' $ "Connecting to bitmex: wss://www.bitmex.com:443" <> T.pack bitmexURI

  --   retriesVar <- newIORef 1

  --   let retry :: IO ()
  --       retry = do
  --         retries <- readIORef retriesVar
  --         writeIORef retriesVar (retries * 2)
  --         -- TODO fetch historical data upon reconnection
  --         when envDevelopment $ warn' $ "Retrying BitMex connection in " <> T.pack (show retries) <> " seconds..."
  --         threadDelay (1000000 * retries)

  --         go `catch` handleBadHandshake
  --            `catch` handleBrokenConnection

  --       handleBrokenConnection :: ConnectionException -> IO ()
  --       handleBrokenConnection e = case e of
  --         ConnectionClosed -> do
  --           when envDevelopment $ warn' $ "Couldn't connect to bitmex: " <> T.pack (show e)
  --           retry
  --         _ -> throwM e

  --       handleBadHandshake :: HandshakeException -> IO ()
  --       handleBadHandshake e = do
  --         when envDevelopment $ warn' $ "Couldn't connect to bitmex: " <> T.pack (show e)
  --         retry

  --       go =  runSecureClient "www.bitmex.com" 443 bitmexURI
  --               (runClientAppT (fmap runSingleton . runInBase)
  --                 (bitmexClient retriesVar (readOnly bitmexInputImpl) (writeOnly bitmexOutput)))

  --   go `catch` handleBadHandshake
  --      `catch` handleBrokenConnection


  -- Control
  do  ControlReceivers
        { controlIncomingUnauth
        , controlIncomingAuth
        } <- control ControlSenders
                { controlOutgoingUnauth = \sid x ->
                    liftIO $ atomically $ TMapChan.insert outgoingUnauth sid x
                , controlOutgoingAuth = \uid x ->
                    liftIO $ atomically $ TMapChan.insert outgoingAuth uid x
                }
      void $ liftBaseWith $ \runInBase -> do
        void $ async $ forever $ do
          when envDevelopment $ log' "Waiting for incomingUnauth..."
          (sid,x) <- atomically $ readTChanRW incomingUnauth
          when envDevelopment $ log' $ "Got incomingUnauth: " <> T.pack (show (sid,x))
          runInBase $ controlIncomingUnauth sid x
        void $ async $ forever $ do
          (uid,x) <- atomically $ readTChanRW incomingAuth
          runInBase $ controlIncomingAuth uid x


  -- HTTP Server
  liftBaseWith $ \runInBase ->
    runEnv port $ runApplicationT (fmap runSingleton . runInBase) $
      httpServer
        (writeOnly incomingUnauth, outgoingUnauth)
        (writeOnly incomingAuth, outgoingAuth)
        challenges loginSessions defApp



defApp :: ApplicationT AppM
defApp _ respond = respond $ textOnly "404" status404 []
