{-# LANGUAGE
    NamedFieldPuns
  #-}

module Server.Subs where

import LocalCooking.Auth (SessionID)
import Types.Env (Env (..), Threads (..), cancelAllSubs, LocalCookingSubsPerSession (..))
import Types (AppM)

import Data.Default (def)
import Data.Singleton.Class (Extractable (..))
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (atomically, readTVar, writeTVar, modifyTVar')
import Control.Monad.Trans.Control.Aligned (MonadBaseControl (..))



addSession :: SessionID -> AppM ()
addSession sid = do
  Env {envThreads = Threads {thSubs}} <- ask
  ts <- liftIO $ atomically $ readTVar thSubs
  case HashMap.lookup sid ts of
    Nothing -> liftIO $ atomically $ writeTVar thSubs $ HashMap.insert sid def ts
    Just _  -> pure ()

delSession :: SessionID -> AppM ()
delSession sid = do
  Env {envThreads = Threads {thSubs}} <- ask
  ts <- liftIO $ atomically $ readTVar thSubs
  case HashMap.lookup sid ts of
    Nothing -> pure ()
    Just subs -> liftIO $ do
      cancelAllSubs subs
      atomically $ writeTVar thSubs $ HashMap.delete sid ts


subscribeOrders :: SessionID -> AppM () -> AppM ()
subscribeOrders k x = do
  addSession k
  Env{envThreads = Threads{thSubs}} <- ask
  ts <- liftIO $ atomically $ readTVar thSubs
  let go s@LocalCookingSubsPerSession{koreDEXSubsOrders} = do
        case koreDEXSubsOrders of
          Nothing -> pure ()
          Just thread -> liftIO (cancel thread)
        liftBaseWith $ \runInBase -> do
          thread <- async (() <$ runInBase x)
          atomically $ writeTVar thSubs $
            HashMap.insert k s{koreDEXSubsOrders = Just thread} ts
  case HashMap.lookup k ts of
    Just s -> go s
    Nothing -> go def

unsubscribeOrders :: SessionID -> AppM ()
unsubscribeOrders k = do
  Env{envThreads = Threads {thSubs}} <- ask
  ts <- liftIO $ atomically $ readTVar thSubs
  case HashMap.lookup k ts of
    Nothing -> pure ()
    Just LocalCookingSubsPerSession{koreDEXSubsOrders} -> case koreDEXSubsOrders of
      Nothing -> pure ()
      Just thread -> liftIO (cancel thread)
