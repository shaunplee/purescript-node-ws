module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), bracket, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Ref as Ref
import Node.Buffer as Buf
import Node.HTTP as HTTP
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Node.WebSocket.Ws as WS
import Node.WebSocket.BinaryType (BinaryType(..))
import Node.WebSocket.WsServer as WsServer

startServerOnPort ::
  Int ->
  (WS.WebSocketConnection -> HTTP.Request -> Effect Unit) ->
  Aff WsServer.WebSocketServer
startServerOnPort p handleConnection =
  liftEffect do
    server <-
      WsServer.createWebSocketServerWithPort
        (WsServer.Port p)
        {}
        (\_ -> log "server started")
    WsServer.onConnection server handleConnection
    WsServer.onServerError server handleError
    pure server

startServerOnHTTPServer ::
  (WS.WebSocketConnection -> HTTP.Request -> Effect Unit) ->
  Aff WsServer.WebSocketServer
startServerOnHTTPServer handleConnection =
  liftEffect do
    server <- HTTP.createServer (\_ _ -> log "dummy")
    HTTP.listen server
      { backlog: Nothing, hostname: "localhost", port: 9001 }
      (pure unit)
    ws <- WsServer.createWebSocketServerWithServer server {}
    WsServer.onConnection ws handleConnection
    WsServer.onServerError ws handleError
    WsServer.onServerClose ws (HTTP.close server (log "node server closed"))
    pure ws

stopServer :: WsServer.WebSocketServer -> Aff Unit
stopServer ws = liftEffect $ WsServer.closeServer ws (log "server closed")

withServer :: Aff WsServer.WebSocketServer -> Aff Unit -> Aff Unit
withServer ws action = bracket ws stopServer (const action)

handleError :: Error -> Effect Unit
handleError err = log $ show err

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Ws client" do
          let
            echoAddress = "ws://echo.websocket.org"
          describe "smoke test" do
            it "can create a WebSocket connection to the websocket.org echo server"
              $ liftEffect do
                  _ <- WS.create echoAddress [] {}
                  pure unit
          describe "connection properties tests" do
            it "can get the url from a WebSocketConnection"
              $ liftEffect do
                  connection <- WS.create echoAddress [] {}
                  address <- WS.url connection
                  address `shouldEqual` echoAddress
        -- describe "communications tests" do
        --   it "can send a message to the websocket.org echo server"
        --     $ do
        --         let
        --           message = "Hello world!"
        --         sentinel <- liftEffect $ Ref.new false
        --         connection <- liftEffect $ WS.create echoAddress [] {}
        --         liftEffect
        --           $ WS.onError connection (\err -> log $ "error: " <> show err)
        --         liftEffect $ WS.onOpen connection
        --           $ do
        --               log "connection opened"
        --               WS.sendString connection message
        --               log $ "message \"" <> message <> "\" sent"
        --               Ref.write true sentinel
        --         delay (Milliseconds 1500.0)
        --         sendComplete <- liftEffect $ Ref.read sentinel
        --         shouldEqual sendComplete true
        --   it "can receive a message from the websocket.org echo server"
        --     $ do
        --         let
        --           message = "Hello world!"
        --         connection <- liftEffect $ WS.create echoAddress [] {}
        --         messageReceived <- liftEffect $ Ref.new ""
        --         liftEffect
        --           $ WS.onMessage connection
        --               ( \(WS.WebSocketMessage msg) -> do
        --                   log $ "message \"" <> message <> "\" received"
        --                   Ref.write msg messageReceived
        --               )
        --         liftEffect
        --           $ WS.onError connection (\err -> log $ "error: " <> show err)
        --         liftEffect
        --           $ WS.onOpen connection
        --               ( do
        --                   log "connection opened"
        --                   WS.sendString connection message
        --                   log $ "message \"" <> message <> "\" sent"
        --               )
        --         delay (Milliseconds 1500.0)
        --         received <- liftEffect $ Ref.read messageReceived
        --         shouldEqual message received
        describe "Ws server" do
          describe "smoke test" do
            it "can start server on port 9000" do
              server <- startServerOnPort 9000 (\_ _ -> log "received message")
              liftEffect $ WsServer.closeServer server (log "server closed")
              pure unit
            it "can start server on running Node HTTP server" do
              _ <- startServerOnHTTPServer (\_ _ -> log "received message")
              pure unit
          describe "receive messages" do
            it "can roundtrip a string message between local client and local echo server"
              $ do
                  let
                    message = "Hello world!"
                  server <-
                    startServerOnPort 9002 echoMessage
                  connection <- liftEffect $ WS.create "ws://localhost:9002" [] {}
                  msgRef <- liftEffect $ Ref.new ""
                  liftEffect $ WS.onMessage connection $ receiveStringMessage msgRef
                  liftEffect
                    $ WS.onError connection (\err -> log $ "error: " <> show err)
                  liftEffect
                    $ WS.onOpen connection
                        ( do
                            log "connection opened"
                            WS.sendString connection message
                            log $ "message \"" <> message <> "\" sent"
                        )
                  delay (Milliseconds 1000.0)
                  received <- liftEffect $ Ref.read msgRef
                  liftEffect $ WsServer.closeServer server (log "server closed")
                  shouldEqual message received
            it "can roundtrip a Buffer message between local client and local echo server"
              $ do
                  let
                    message = [ 1, 2, 3 ] :: Array Buf.Octet
                  buf <- liftEffect $ (Buf.fromArray message :: Effect Buf.Buffer)
                  server <-
                    startServerOnPort 9003 echoMessage
                  connection <- liftEffect $ WS.create "ws://localhost:9003" [] {}
                  liftEffect $ WS.setBinaryType connection Buffer
                  msgRef <- liftEffect $ Ref.new ([] :: Array Buf.Octet)
                  liftEffect $ WS.onMessage connection $ receiveBufferMessage msgRef
                  liftEffect $ WS.onError connection (\err -> log $ "error: " <> show err)
                  liftEffect
                    $ WS.onOpen connection
                        ( do
                            log "binary connection opened"
                            WS.setBinaryType connection Buffer
                            WS.sendBuffer connection buf
                            log $ "buffer message \"" <> show message <> "\" sent"
                        )
                  delay (Milliseconds 1000.0)
                  received <- liftEffect $ Ref.read msgRef
                  liftEffect $ WsServer.closeServer server (log "binary server closed")
                  shouldEqual message received
            it "can roundtrip an ArrayBuffer message between local client and local echo server"
              $ do
                  let
                    message = [ 1, 2, 3 ] :: Array Buf.Octet
                  buf <- liftEffect $ (Buf.fromArray message :: Effect Buf.Buffer)
                  arrBuf <- liftEffect $ Buf.toArrayBuffer buf
                  server <-
                    startServerOnPort 9003 echoMessage
                  connection <- liftEffect $ WS.create "ws://localhost:9003" [] {}
                  liftEffect $ WS.setBinaryType connection ArrayBuffer
                  msgRef <- liftEffect $ Ref.new ([] :: Array Buf.Octet)
                  liftEffect $ WS.onMessage connection $ receiveArrayBufferMessage msgRef
                  liftEffect $ WS.onError connection (\err -> log $ "error: " <> show err)
                  liftEffect
                    $ WS.onOpen connection
                        ( do
                            log "binary connection opened"
                            WS.setBinaryType connection ArrayBuffer
                            WS.sendArrayBuffer connection arrBuf
                            log $ "buffer message \"" <> show message <> "\" sent"
                        )
                  delay (Milliseconds 1000.0)
                  received <- liftEffect $ Ref.read msgRef
                  liftEffect $ WsServer.closeServer server (log "binary server closed")
                  shouldEqual message received

echoMessage :: WS.WebSocketConnection -> HTTP.Request -> Effect Unit
echoMessage conn _ =
  WS.onMessage conn
    ( case _ of
        (WS.WebSocketStringMessage msg) -> WS.sendString conn msg
        (WS.WebSocketBinaryBufferMessage msg) -> do
          WS.setBinaryType conn Buffer
          WS.sendBuffer conn msg
        (WS.WebSocketBinaryArrayBufferMessage msg) -> WS.sendArrayBuffer conn msg
    )

receiveStringMessage :: Ref.Ref String -> WS.WebSocketMessage -> Effect Unit
receiveStringMessage msgRef wsm = case wsm of
  (WS.WebSocketStringMessage msg) -> do
    log $ "message \"" <> msg <> "\" received"
    Ref.write msg msgRef
  (WS.WebSocketBinaryBufferMessage _) ->
    fail
      $ "error: received a Buffer message but expected an ArrayBuffer"
  (WS.WebSocketBinaryArrayBufferMessage _) ->
    fail
      $ "error: received an ArrayBuffer message but expected an ArrayBuffer"

receiveBufferMessage :: Ref.Ref (Array Buf.Octet) -> WS.WebSocketMessage -> Effect Unit
receiveBufferMessage msgRef wsm = case wsm of
  (WS.WebSocketStringMessage _) ->
    fail
      $ "error: received a String message but expected an ArrayBuffer"
  (WS.WebSocketBinaryBufferMessage msgBuf) -> do
    msg <- Buf.toArray msgBuf :: Effect (Array Buf.Octet)
    log $ "buffer message \"" <> show msg <> "\" received"
    Ref.write msg msgRef
  (WS.WebSocketBinaryArrayBufferMessage _) ->
    fail
      $ "error: received an ArrayBuffer message but expected an ArrayBuffer"

receiveArrayBufferMessage :: Ref.Ref (Array Buf.Octet) -> WS.WebSocketMessage -> Effect Unit
receiveArrayBufferMessage msgRef wsm = case wsm of
  (WS.WebSocketStringMessage _) ->
    fail
      $ "error: received a String message but expected an ArrayBuffer"
  (WS.WebSocketBinaryBufferMessage _) ->
    fail
      $ "error: received a Buffer message but expected an ArrayBuffer"
  (WS.WebSocketBinaryArrayBufferMessage msgArrBuf) -> do
    msgBuf <- Buf.fromArrayBuffer msgArrBuf :: Effect Buf.Buffer
    msg <- Buf.toArray msgBuf :: Effect (Array Buf.Octet)
    log $ "arraybuffer message \"" <> show msg <> "\" received"
    Ref.write msg msgRef
