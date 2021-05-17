module WebSocket.Ws where

import Prelude
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried
  ( EffectFn1
  , EffectFn2
  , EffectFn3
  , mkEffectFn1
  , mkEffectFn2
  , runEffectFn2
  , runEffectFn3
  )
import Data.Newtype (class Newtype)
import Record (insert)
import Data.Symbol (SProxy(..))
import Node.HTTP (Request, Server)
import Type.Row (class Cons, class Lacks, class Union)

-- | The type of a WebSocket server object
foreign import data WebSocketServer :: Type

foreign import data WebSocketConnection :: Type

newtype WebSocketMessage
  = WebSocketMessage String

derive newtype instance showWSM :: Show WebSocketMessage

derive instance newtypeWSM :: Newtype WebSocketMessage _

-- | The effect associated with using the WebSocket module
foreign import data WS :: Effect

newtype Protocol
  = Protocol String

derive newtype instance eqProtocol :: Eq Protocol

derive newtype instance ordProtocol :: Ord Protocol

derive instance newtypeProtocol :: Newtype Protocol _

-- TODO: more options from:
-- https://github.com/websockets/ws/blob/master/doc/ws.md
type WebSocketServerOptions
  = ( host :: String
    , backlog :: Int
    )

-- | The port to listen on if calling createWebSocketServerWithPort
newtype Port
  = Port Int

newtype CloseCode
  = CloseCode Int

newtype CloseReason
  = CloseReason String

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

foreign import onMessage_ ::
  EffectFn2
    WebSocketConnection
    (EffectFn1 WebSocketMessage Unit)
    Unit

-- | Attaches a message event handler to a WebSocketConnection
onMessage ::
  WebSocketConnection ->
  (WebSocketMessage -> Effect Unit) ->
  Effect Unit
onMessage ws callback = runEffectFn2 onMessage_ ws (mkEffectFn1 callback)

foreign import onClose_ ::
  EffectFn2
    WebSocketConnection
    (EffectFn2 CloseCode CloseReason Unit)
    Unit

-- | Attaches a close event handler to a WebSocketConnection
onClose ::
  WebSocketConnection ->
  (CloseCode -> CloseReason -> Effect Unit) ->
  Effect Unit
onClose ws callback = runEffectFn2 onClose_ ws (mkEffectFn2 callback)

foreign import onError_ ::
  EffectFn2
    WebSocketConnection
    (EffectFn1 Error Unit)
    Unit

-- | Attaches an error event handler to a WebSocketConnection
onError ::
  WebSocketConnection ->
  (Error -> Effect Unit) ->
  Effect Unit
onError ws callback = runEffectFn2 onError_ ws (mkEffectFn1 callback)

foreign import sendMessage_ ::
  EffectFn2
    WebSocketConnection
    WebSocketMessage
    Unit

-- | Send a message over a WebSocketConnection
sendMessage ::
  WebSocketConnection ->
  WebSocketMessage ->
  Effect Unit
sendMessage ws message = runEffectFn2 sendMessage_ ws message

foreign import close_ ::
  EffectFn3
    WebSocketConnection
    CloseCode
    CloseReason
    Unit

-- | Initiate a closing handshake
close ::
  WebSocketConnection ->
  Effect Unit
close ws = runEffectFn3 close_ ws (CloseCode 1000) (CloseReason "Closed by server") -- 1000 is the CloseCode for normal closure

-- | Initiate a closing handshake with given code and reason
close' ::
  WebSocketConnection ->
  CloseCode ->
  CloseReason ->
  Effect Unit
close' ws code reason = runEffectFn3 close_ ws code reason
