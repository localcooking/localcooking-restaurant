module Client where

import LocalCooking.Auth (SessionID (..))
import LocalCooking.WebSocket (LocalCookingOutput (..), LocalCookingInput (..))

import Prelude

import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Time.Duration (Milliseconds (..))
import Data.UUID (GENUUID, genUUID)
import Data.URI.URI (URI, print) as URI
import Data.Argonaut (decodeJson, encodeJson, jsonParser)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER, setTimeout, clearTimeout, setInterval, clearInterval)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import WebSocket (newWebSocket, WEBSOCKET)
import Queue.One (Queue, READ, newQueue, onQueue, putQueue, delQueue)



type Effects eff =
  ( ws        :: WEBSOCKET
  , console   :: CONSOLE
  , uuid      :: GENUUID
  , timer     :: TIMER
  , ref       :: REF
  , exception :: EXCEPTION
  | eff)


client :: forall eff
        . { uri :: SessionID -> URI.URI
          , toLocalCooking :: Queue (read :: READ) (Effects eff) LocalCookingInput
          }
       -> Eff (Effects eff) Unit
client {uri,toLocalCooking} = do
  backoffSignal <- newQueue

  waitingThread <- newRef Nothing
  waitedSoFar <- newRef Nothing
  pingingThread <- newRef Nothing

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
                mPinger <- takeRef pingingThread
                case mPinger of
                  Nothing -> pure unit
                  Just pinger -> clearInterval pinger
                delQueue toLocalCooking
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
                mPinger <- takeRef pingingThread
                case mPinger of
                  Nothing -> pure unit
                  Just pinger -> clearInterval pinger
                delQueue toLocalCooking
                delay' <- readRef0 waitedSoFar
                let delay'' = delay' * 2
                warn $ "Connection to WebSocket errored - reason: "
                    <> error
                    <> ", reconnecting in " <> show delay'' <> " seconds."
                putQueue backoffSignal delay''
            , onmessage: \{send,close} msg ->
                if msg == "\"pong\""
                  then pure unit
                  else case jsonParser msg >>= decodeJson of
                    Left e -> throw e
                    Right (x :: LocalCookingOutput) -> pure unit
            , onopen: \{send,close} -> do
                log "Connected to WebSocket"
                writeRef waitedSoFar Nothing
                pinger <- setInterval 3000 $ do -- FIXME lock pinging with output queue for throttling
                  send $ show $ encodeJson "ping"
                writeRef pingingThread (Just pinger)
                onQueue toLocalCooking (send <<< show <<< encodeJson)
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
