module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Node.HTTP as HTTP
import Test.Spec (around_, describe, it, pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import WebSocket.Ws as WS

startServerOnPort ::
  Int ->
  (WS.WebSocketConnection -> HTTP.Request -> Effect Unit) ->
  Aff WS.WebSocketServer
startServerOnPort p handleConnection =
  liftEffect do
    server <- WS.createWebSocketServerWithPort (WS.Port p) {} (\_ -> log "server started")
    WS.onConnection server handleConnection
    WS.onServerError server handleError
    pure server

startServerOnHTTPServer ::
  (WS.WebSocketConnection -> HTTP.Request -> Effect Unit) ->
  Aff WS.WebSocketServer
startServerOnHTTPServer handleConnection =
  liftEffect do
    server <- HTTP.createServer (\_ _ -> log "dummy")
    HTTP.listen server
      { backlog: Nothing, hostname: "localhost", port: 9000 }
      (pure unit)
    ws <- WS.createWebSocketServerWithServer server {}
    WS.onConnection ws handleConnection
    WS.onServerError ws handleError
    pure ws

stopServer :: WS.WebSocketServer -> Aff Unit
stopServer _ = pure unit

withServer :: Aff WS.WebSocketServer -> Aff Unit -> Aff Unit
withServer ws action = bracket ws stopServer (const action)

handleError :: Error -> Effect Unit
handleError err = log $ show err

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Ws server" do
          describe "smoke test" do
            it "can start server on port 9000" do
              _ <- startServerOnPort 9000 (\_ _ -> log "received message")
              pure unit
            it "can start server on running Node HTTP server" do
              _ <- startServerOnHTTPServer (\_ _ -> log "received message")
              pure unit
          describe "receive messages"
            $ around_ (withServer (startServerOnPort 9000 (\_ _ -> log "received message"))) do
                pending "should receive a message"
