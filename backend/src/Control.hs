{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  #-}

module Control where

import Types (AppM)
import Types.Env (Env (..), Database (..), Threads (..), isDevelopment)
import LocalCooking.Auth (UserID, SessionID)
import LocalCooking.WebSocket (LocalCookingInput (..), LocalCookingOutput (..))
import LocalCooking.Subs (SubsInput (..), SubsOutput (..))

import Data.Monoid ((<>))
import qualified Data.Text as T
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Logging (log')



data ControlSenders = ControlSenders
  { controlOutgoingUnauth :: SessionID -> LocalCookingOutput -> AppM ()
  , controlOutgoingAuth   :: UserID -> LocalCookingOutput -> AppM ()
  }

data ControlReceivers = ControlReceivers
  { controlIncomingUnauth :: SessionID -> LocalCookingInput -> AppM ()
  , controlIncomingAuth   :: UserID -> LocalCookingInput -> AppM ()
  }

control :: ControlSenders -> AppM ControlReceivers
control ControlSenders{controlOutgoingUnauth,controlOutgoingAuth} = do
  -- subscriptions
  -- controlBitmexInput $ Subscribe $ TradeBin1m $ Just XBTUSD
  -- controlBitmexInput $ Subscribe $ OrderBook10 $ Just XBTUSD -- FIXME should we batch ourselves?

  env <- ask


  pure ControlReceivers
    { controlIncomingUnauth = \sid input -> do
        case input of
          -- LocalCookingOp op -> case op of -- FIXME subscriptions
          --   LocalCookingOpChartData -> do
          --     xs <- atomically $ readTVar statsDB
          --     atomically $ TMapChan.insert controlOutgoingUnauth sid $
          --       LocalCookingOpResult $ LocalCookingOpResultChartData xs
          -- LocalCookingSubsInput x' -> case x' of
          _ -> when (isDevelopment env) $ liftIO $ log' $ "Got input that's not handled: " <> T.pack (show input) <> ", from session: " <> T.pack (show sid)
        when (isDevelopment env) $ liftIO $ log' "Finished reading controlIncomingUnauth"
    , controlIncomingAuth = \_ _ -> pure ()
    }
