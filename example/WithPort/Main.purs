module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.HTTP (Request)
import Node.WebSocket.Ws (WebSocketConnection, WebSocketMessage, close, onError, onMessage, sendString)
import Node.WebSocket.WsServer (Port(Port), createWebSocketServerWithPort, onConnection, onServerError)

handleMessage :: WebSocketConnection -> WebSocketMessage -> Effect Unit
handleMessage ws msg = do
  log $ show msg
  close ws

handleError :: Error -> Effect Unit
handleError err = do
  log $ show err

handleConnection :: WebSocketConnection -> Request -> Effect Unit
handleConnection ws _ = do
  log "Connected!"
  onMessage ws $ handleMessage ws
  onError ws handleError
  sendString ws "Hello, world!"

main :: Effect Unit
main = do
  wss <-
    createWebSocketServerWithPort (Port 8080) {}
      $ const do
          log "Listening on port 8080."
  onConnection wss handleConnection
  onServerError wss handleError
