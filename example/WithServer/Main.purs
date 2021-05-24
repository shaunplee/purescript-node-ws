module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Data.Maybe (Maybe(..))
import Node.HTTP (Request, Response, createServer, responseAsStream, setStatusCode, listen)
import Node.Stream (end)
import Node.WebSocket.Ws (WebSocketConnection, WebSocketMessage, close, onError, onMessage, sendString)
import Node.WebSocket.WsServer (createWebSocketServerWithServer, onConnection, onServerError)

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

-- this is an extra callback needed by Node.HTTP createServer
respond :: Request -> Response -> Effect Unit
respond _ res = do
  setStatusCode res 200
  let
    outputStream = responseAsStream res
  end outputStream (pure unit)

main :: Effect Unit
main = do
  server <- createServer respond
  wss <- createWebSocketServerWithServer server {}
  onConnection wss handleConnection
  onServerError wss handleError
  listen server { hostname: "localhost", port: 8080, backlog: Nothing }
    $ void do
        log "Listening on port 8080."
