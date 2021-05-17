module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), bracket, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Node.HTTP as HTTP
import Test.Spec (around_, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import WebSocket.Ws as WS
import WebSocket.WsClient as WsClient

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
      { backlog: Nothing, hostname: "localhost", port: 9001 }
      (pure unit)
    ws <- WS.createWebSocketServerWithServer server {}
    WS.onConnection ws handleConnection
    WS.onServerError ws handleError
    WS.onServerClose ws (HTTP.close server (log "node server closed"))
    pure ws

stopServer :: WS.WebSocketServer -> Aff Unit
stopServer ws = liftEffect $ WS.closeServer ws (log "server closed")

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
              server <- startServerOnPort 9000 (\_ _ -> log "received message")
              liftEffect $ WS.closeServer server (log "server closed")
              pure unit
            it "can start server on running Node HTTP server" do
              _ <- startServerOnHTTPServer (\_ _ -> log "received message")
              pure unit
        describe "Ws client" do
          let
            echoAddress = "ws://echo.websocket.org"
          describe "smoke test" do
            it "can create a WebSocket connection to the websocket.org echo server"
              $ liftEffect do
                  _ <- WsClient.create "ws://echo.websocket.org" [] {}
                  pure unit
          describe "connection properties tests" do
            it "can get the url from a WebSocketConnection"
              $ liftEffect do
                  connection <- WsClient.create echoAddress [] {}
                  address <- WsClient.url connection
                  address `shouldEqual` echoAddress
          describe "communications tests" do
            it "can send a message to the websocket.org echo server"
              $ do
                  let
                    message = "Hello world!"
                  connection <- liftEffect $ WsClient.create echoAddress [] {}
                  liftEffect
                    $ WsClient.onError connection (\err -> log $ "error: " <> show err)
                  liftEffect
                    $ WsClient.onOpen connection
                        ( do
                            log "connection opened"
                            WsClient.sendString connection message
                            log $ "message \"" <> message <> "\" sent"
                        )
                  delay (Milliseconds 1000.0)
                  pure unit
            it "can receive a message from the websocket.org echo server"
              $ do
                  let
                    message = "Hello world!"
                  connection <- liftEffect $ WsClient.create echoAddress [] {}
                  liftEffect
                    $ WsClient.onMessage connection
                        ( \(WS.WebSocketMessage msg) -> log $ "received: " <> msg
                        )
                  liftEffect
                    $ WsClient.onError connection (\err -> log $ "error: " <> show err)
                  liftEffect
                    $ WsClient.onOpen connection
                        ( do
                            log "connection opened"
                            WsClient.sendString connection message
                            log $ "message \"" <> message <> "\" sent"
                        )
                  delay (Milliseconds 1000.0)
                  pure unit
          describe "receive messages"
            $ around_
                (withServer (startServerOnPort 9000 (\_ _ -> log "received message"))) do
                pending "can roundtrip a message with a local echo server"
