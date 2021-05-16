module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.HTTP as HTTP
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import WebSocket.Ws as WS

startServerOnPort :: Int -> Aff WS.WebSocketServer
startServerOnPort p = liftEffect $ WS.createWebSocketServerWithPort (WS.Port p) {} (\_ -> log "server started")

startServerOnHTTPServer :: Aff WS.WebSocketServer
startServerOnHTTPServer =
  liftEffect do
    server <- HTTP.createServer (\_ _ -> log "dummy")
    HTTP.listen server
      { backlog: Nothing, hostname: "localhost", port: 9000 }
      (pure unit)
    WS.createWebSocketServerWithServer server {}

stopServer :: WS.WebSocketServer -> Aff Unit
stopServer _ = pure unit

withServer :: Aff WS.WebSocketServer -> Aff Unit -> Aff Unit
withServer ws action = bracket ws stopServer (const action)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Ws server" do
          describe "smoke test" do
            it "can start server on port 9000" do
              _ <- startServerOnPort 9000
              pure unit
            it "can start server on running Node HTTP server" do
              _ <- startServerOnHTTPServer
              pure unit
