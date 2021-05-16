module WebSocket.WsClient where

import Prelude
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Partial.Unsafe (unsafePartial)
import Web.File.Blob (Blob)
import WebSocket.Ws (WebSocketConnection, WebSocketMessage, WS)
import WebSocket.BinaryType (BinaryType(..), printBinaryType)
import WebSocket.ReadyState (ReadyState(..), toEnumReadyState)

newtype Protocol
  = Protocol String

newtype CloseCode
  = CloseCode Int

newtype CloseReason
  = CloseReason String

derive newtype instance eqProtocol :: Eq Protocol

derive newtype instance ordProtocol :: Ord Protocol

derive instance newtypeProtocol :: Newtype Protocol _

foreign import createWebSocketConnection_ :: forall options. EffectFn3 String (Array Protocol) options WebSocketConnection

-- | Creates a WebSocketConnection to a given address (URL), Array of
-- | subprotocols, and `options` see
-- | https://github.com/websockets/ws/blob/master/doc/ws.md for list
-- | of options
create ::
  forall options.
  String ->
  Array Protocol ->
  { | options } ->
  Effect WebSocketConnection
create address protocols options = runEffectFn3 createWebSocketConnection_ address protocols options

foreign import url :: WebSocketConnection -> Effect String

foreign import readyStateImpl :: WebSocketConnection -> Effect Int

readyState :: WebSocketConnection -> Effect ReadyState
readyState ws = do
  rs <- readyStateImpl ws
  pure $ unsafePartial $ fromJust $ toEnumReadyState rs

foreign import bufferedAmount :: WebSocketConnection -> Effect Number

foreign import extensions :: WebSocketConnection -> Effect String

foreign import protocol :: WebSocketConnection -> Effect String

foreign import close :: WebSocketConnection -> Effect Unit

foreign import getBinaryTypeImpl :: WebSocketConnection -> Effect String

foreign import setBinaryTypeImpl :: WebSocketConnection -> String -> Effect Unit

getBinaryType :: WebSocketConnection -> Effect BinaryType
getBinaryType ws =
  unsafePartial do
    getBinaryTypeImpl ws
      <#> case _ of
          "blob" -> Blob
          "arraybuffer" -> ArrayBuffer

setBinaryType :: WebSocketConnection -> BinaryType -> Effect Unit
setBinaryType ws = setBinaryTypeImpl ws <<< printBinaryType

sendString :: WebSocketConnection -> String -> Effect Unit
sendString = sendImpl

sendBlob :: WebSocketConnection -> Blob -> Effect Unit
sendBlob = sendImpl

sendArrayBuffer :: WebSocketConnection -> ArrayBuffer -> Effect Unit
sendArrayBuffer = sendImpl

sendArrayBufferView :: forall t. WebSocketConnection -> ArrayView t -> Effect Unit
sendArrayBufferView = sendImpl

foreign import sendImpl :: forall a. WebSocketConnection -> a -> Effect Unit

connectionReady :: WebSocketConnection -> Effect Boolean
connectionReady ws = do
  rs <- readyState ws
  pure $ rs == Open

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

foreign import onOpen_ ::
  EffectFn2
    WebSocketConnection
    (Effect Unit)
    Unit

-- | Attaches a close event handler to a WebSocketConnection
onOpen ::
  WebSocketConnection ->
  (Effect Unit) ->
  Effect Unit
onOpen ws callback = runEffectFn2 onOpen_ ws callback
