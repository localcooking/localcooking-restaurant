{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  #-}

module Control where

import Types (AppM)
import Types.Env (Env (..), Database (..), Threads (..))
import LocalCooking.Auth (UserID, SessionID)
import LocalCooking.WebSocket (LocalCookingInput (..), LocalCookingOutput (..))
import LocalCooking.Subs (SubsInput (..), CandleStickInput (..), SubsOutput (..), CandleStickOutput (..))

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

  Env{ envDevelopment
     } <- ask


  pure ControlReceivers
    { controlIncomingUnauth = \sid input -> do
        case input of
          -- LocalCookingOp op -> case op of -- FIXME subscriptions
          --   LocalCookingOpChartData -> do
          --     xs <- atomically $ readTVar statsDB
          --     atomically $ TMapChan.insert controlOutgoingUnauth sid $
          --       LocalCookingOpResult $ LocalCookingOpResultChartData xs
          LocalCookingSubsInput x' -> case x' of
            CandleStickInput x'' -> case x'' of
              CandleStickSubscribe -> do
                when envDevelopment $ liftIO $ log' $ "Got CandleStick subscription from: " <> T.pack (show sid)
                -- xs <- liftIO $ atomically $ readTVar statsDB
                -- when envDevelopment $ liftIO $ log' $ "Writing init... " <> T.pack (show xs)
                -- controlOutgoingUnauth sid $ LocalCookingSubsOutput $ CandleStickOutput $ CandleStickInit xs
                when envDevelopment $ liftIO $ log' "Wrote init"
                -- subscribeCandleStick sid $ \x -> do
                --   when envDevelopment $ liftIO $ log' "Writing insert..."
                --   controlOutgoingUnauth sid $ LocalCookingSubsOutput $ CandleStickOutput $ CandleStickAdd x
                --   when envDevelopment $ liftIO $ log' "Wrote insert..."
                when envDevelopment $ liftIO $ log' "Subscribed candlestick"
              CandleStickUnsubscribe -> do
                when envDevelopment $ liftIO $ log' $ "Got CandleStick unsubscription from: " <> T.pack (show sid)
                -- unsubscribeCandleStick sid
          _ -> when envDevelopment $ liftIO $ log' $ "Got input that's not handled: " <> T.pack (show input) <> ", from session: " <> T.pack (show sid)
        when envDevelopment $ liftIO $ log' "Finished reading controlIncomingUnauth"
    , controlIncomingAuth = \_ _ -> pure ()
    }
