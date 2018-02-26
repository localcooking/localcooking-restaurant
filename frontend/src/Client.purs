module Client where

import LocalCooking.Auth (SessionID (..))

import Prelude

import Data.Maybe (Maybe (..))
import Data.Time.Duration (Milliseconds (..))
import Data.UUID (GENUUID, genUUID)
import Data.URI.URI (URI, print) as URI
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER, setTimeout, clearTimeout)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import WebSocket (newWebSocket, WEBSOCKET)
import Queue.One (newQueue, onQueue, putQueue)



client :: forall eff
        . { uri :: SessionID -> URI.URI
          }
       -> Eff ( ws :: WEBSOCKET
              , console :: CONSOLE
              , uuid :: GENUUID
              , timer :: TIMER
              , ref :: REF
              | eff) Unit
client {uri} = do
  backoffSignal <- newQueue

  waitingThread <- newRef Nothing
  waitedSoFar <- newRef Nothing

  onQueue backoffSignal \delay -> do
    sessionID <- SessionID <$> genUUID

    mOldThread <- takeRef waitingThread
    case mOldThread of
      Nothing -> pure unit
      Just oldThread -> clearTimeout oldThread -- destoys potentially existing websocket until the delay is over

    thread <- setTimeout (delay * 1000) $ do
      writeRef waitedSoFar (Just delay)
      newWebSocket
        { url: URI.print (uri sessionID)
        , protocols: []
        , continue: \_ ->
            { onclose: \{code,reason,wasClean} -> do
                delay' <- readRef0 waitedSoFar
                let delay'' = delay' * 2
                warn $ "Connection to WebSocket lost. Code: "
                    <> show code
                    <> ", reason: "
                    <> show reason
                    <> ", was clean: "
                    <> show wasClean
                    <> ", reconnecting in " <> show delay'' <> " seconds."
                putQueue backoffSignal delay''
            , onerror: \error -> do
                delay' <- readRef0 waitedSoFar
                let delay'' = delay' * 2
                warn $ "Connection to WebSocket errored - reason: "
                    <> error
                    <> ", reconnecting in " <> show delay'' <> " seconds."
                putQueue backoffSignal delay''
            , onmessage: \{send,close} msg -> pure unit
            , onopen: \{send,close} -> do
                log "Connected to WebSocket"
                writeRef waitedSoFar Nothing
            }
        }
    writeRef waitingThread (Just thread)

  putQueue backoffSignal 1


readRef0 ref = do
  mX <- readRef ref
  case mX of
    Nothing -> pure 1
    Just x -> pure x

takeRef ref = do
  mX <- readRef ref
  writeRef ref Nothing
  pure mX
