module WebSocket.WsServer where

import Prelude
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn2)
import Record (insert)
import Data.Symbol (SProxy(..))
import Node.HTTP (Request, Server)
import Type.Row (class Cons, class Lacks, class Union)
import WebSocket.Ws (Protocol, WebSocketConnection)

-- | The type of a WebSocket server object
foreign import data WebSocketServer :: Type

-- | The effect associated with using the WebSocket module
foreign import data WS :: Effect

-- TODO: more options from:
-- https://github.com/websockets/ws/blob/master/doc/ws.md
type WebSocketServerOptions
  = ( host :: String
    , port :: Int
    , backlog :: Int
    , server :: Server
    , path :: String
    , handleProtocols :: Array Protocol -> Request -> Effect Boolean
    , noServer :: Boolean
    , clientTracking :: Boolean
    , maxPayload :: Int
    )

-- | The port to listen on if calling createWebSocketServerWithPort
newtype Port
  = Port Int

foreign import createWebSocketServer_ ::
  forall options.
  EffectFn2 options
    (EffectFn1 Unit Unit)
    WebSocketServer

-- | Creates a WebSocket.Server and internally a HTTP server
-- | which binds to a given port
-- |
-- | The supplied callback is called when the created HTTP server
-- | starts listening.
createWebSocketServerWithPort ::
  forall options options' trash.
  Union options options' WebSocketServerOptions =>
  Lacks "port" options =>
  Cons "port" Port options trash =>
  Port ->
  { | options } ->
  (Unit -> Effect Unit) ->
  Effect WebSocketServer
createWebSocketServerWithPort (Port port) options callback = runEffectFn2 createWebSocketServer_ options' callback'
  where
  options' = insert (SProxy :: SProxy "port") port options

  callback' = mkEffectFn1 callback

-- | Creates a WebSocket.Server from a pre-existing Node.Server
createWebSocketServerWithServer ::
  forall options options' trash.
  Union options options' WebSocketServerOptions =>
  Lacks "server" options =>
  Cons "server" Server options trash =>
  Server ->
  { | options } ->
  Effect WebSocketServer
createWebSocketServerWithServer server options = runEffectFn2 createWebSocketServer_ options' callback'
  where
  options' = insert (SProxy :: SProxy "server") server options

  callback' = mkEffectFn1 $ const (pure unit)

foreign import onListening_ ::
  EffectFn2 WebSocketServer (Effect Unit) Unit

-- | Attaches a listening event handler to a WebSocketServer, where the
-- | listening event is emitted when the server has been bound.
onListening :: WebSocketServer -> Effect Unit -> Effect Unit
onListening server callback = runEffectFn2 onListening_ server callback

foreign import onConnection_ ::
  EffectFn2
    WebSocketServer
    (EffectFn2 WebSocketConnection Request Unit)
    Unit

-- | Attaches a connection event handler to a WebSocketServer
onConnection ::
  WebSocketServer ->
  (WebSocketConnection -> Request -> Effect Unit) ->
  Effect Unit
onConnection server callback = runEffectFn2 onConnection_ server (mkEffectFn2 callback)

foreign import onServerError_ ::
  EffectFn2
    WebSocketServer
    (EffectFn1 Error Unit)
    Unit

-- | Attaches an error event handler to a WebSocketServer
onServerError ::
  WebSocketServer ->
  (Error -> Effect Unit) ->
  Effect Unit
onServerError server callback = runEffectFn2 onServerError_ server (mkEffectFn1 callback)

foreign import onHeaders_ ::
  EffectFn2
    WebSocketServer
    (EffectFn2 (Array String) Request Unit)
    Unit

-- | Attaches a headers event handler to a WebSocketServer, where the
-- | headers event is emitted before the response headers are written
-- | to the socket as part of the handshake. This allows you to
-- | inspect/modify the headers before they are sent
onHeaders ::
  WebSocketServer ->
  (Array String -> Request -> Effect Unit) ->
  Effect Unit
onHeaders server callback = runEffectFn2 onHeaders_ server (mkEffectFn2 callback)

foreign import closeServer_ :: EffectFn2 WebSocketServer (Effect Unit) Unit

-- | Shut down the server and run the callback when the shut down is
-- | complete. If an external HTTP server is used, it must be closed
-- | manually.
closeServer :: WebSocketServer -> Effect Unit -> Effect Unit
closeServer server callback = runEffectFn2 closeServer_ server callback

foreign import onServerClose_ ::
  EffectFn2 WebSocketServer (Effect Unit) Unit

-- | Attaches a close event handler to a WebSocketServer, where the
-- | close event is emitted when the server closes.
onServerClose :: WebSocketServer -> Effect Unit -> Effect Unit
onServerClose server callback = runEffectFn2 onServerClose_ server callback
