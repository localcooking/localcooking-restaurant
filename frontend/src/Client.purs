module Client where

import Prelude

import Data.URI.URI (URI, print) as URI
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import WebSocket (newWebSocket, WEBSOCKET)



client :: forall eff
        . { uri :: URI.URI
          }
       -> Eff ( ws :: WEBSOCKET
              , console :: CONSOLE
              | eff) Unit
client {uri} =
  newWebSocket
    { url: URI.print uri
    , protocols: []
    , continue: \_ ->
        { onclose: \{code,reason,wasClean} -> do
            warn $ "Connection to WebSocket lost. Code: "
                <> show code
                <> ", reason: "
                <> show reason
                <> ", was clean: "
                <> show wasClean
        , onerror: \error ->
            warn $ "Connection to WebSocket errored - reason: " <> error
        , onmessage: \{send,close} msg -> pure unit
        , onopen: \{send,close} -> do
            log "Connected to WebSocket"
        }
    }
