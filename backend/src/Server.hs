{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , QuasiQuotes
  , DataKinds
  #-}

module Server where

import Server.HTTP (httpServer)
import Server.Dependencies (servedDependencies)
import Control (control, ControlSenders (..), ControlReceivers (..))
import Types (AppM)
import Types.Env (Env (..), isDevelopment)
import LocalCooking.Auth (UserID, SessionID, ChallengeID (..))
import LocalCooking.WebSocket (LocalCookingInput (..), LocalCookingOutput (..))

import Web.Routes.Nested (textOnly)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Trans (ApplicationT, runApplicationT)
import Network.WebSockets (ConnectionException (..), runClient, HandshakeException)
import Network.WebSockets.Trans (runClientAppT)
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
  env <- ask

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
          when (isDevelopment env) $ log' "Waiting for incomingUnauth..."
          (sid,x) <- atomically $ readTChanRW incomingUnauth
          when (isDevelopment env) $ log' $ "Got incomingUnauth: " <> T.pack (show (sid,x))
          runInBase $ controlIncomingUnauth sid x
        void $ async $ forever $ do
          (uid,x) <- atomically $ readTChanRW incomingAuth
          runInBase $ controlIncomingAuth uid x


  -- HTTP Server
  liftBaseWith $ \runInBase -> do
    dependencies <- fmap runSingleton $ runInBase servedDependencies
    server' <- fmap runSingleton $ runInBase $ runApplicationT $
      httpServer
        (writeOnly incomingUnauth, outgoingUnauth)
        (writeOnly incomingAuth, outgoingAuth)
        challenges loginSessions dependencies defApp
    runEnv port server'



defApp :: ApplicationT AppM
defApp _ respond = respond $ textOnly "404" status404 []
