module WebSocket.Ws where

import Prelude
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
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
import Foreign (Foreign, typeOf, unsafeFromForeign)
import Partial.Unsafe (unsafePartial)
import Node.Buffer (Buffer)
import WebSocket.BinaryType (BinaryType(..), printBinaryType)
import WebSocket.ReadyState (ReadyState, toEnumReadyState)

foreign import data WebSocketConnection :: Type

-- TODO: Add support for receiving additional data types?
-- (Array/Number/Object/TypedArray)
data WebSocketMessage
  = WebSocketStringMessage String
  | WebSocketBinaryBufferMessage Buffer
  | WebSocketBinaryArrayBufferMessage ArrayBuffer

instance showWSM :: Show WebSocketMessage where
  show (WebSocketStringMessage s) = "WebSocketMessage " <> s
  show (WebSocketBinaryBufferMessage _) = "WebSocketMessage <binary buffer>"
  show (WebSocketBinaryArrayBufferMessage _) = "WebSocketMessage <binary arraybuffer>"

-- | The effect associated with using the WebSocket module
foreign import data WS :: Effect

newtype Protocol
  = Protocol String

derive newtype instance eqProtocol :: Eq Protocol

derive newtype instance ordProtocol :: Ord Protocol

derive instance newtypeProtocol :: Newtype Protocol _

newtype CloseCode
  = CloseCode Int

newtype CloseReason
  = CloseReason String

foreign import createWebSocketConnection_ ::
  forall options. EffectFn3 String (Array Protocol) options WebSocketConnection

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
create address protocols options =
  runEffectFn3
    createWebSocketConnection_
    address
    protocols
    options

foreign import url :: WebSocketConnection -> Effect String

foreign import readyStateImpl :: WebSocketConnection -> Effect Int

readyState :: WebSocketConnection -> Effect ReadyState
readyState ws = do
  rs <- readyStateImpl ws
  pure $ unsafePartial $ fromJust $ toEnumReadyState rs

foreign import bufferedAmount :: WebSocketConnection -> Effect Number

foreign import extensions :: WebSocketConnection -> Effect String

foreign import protocol :: WebSocketConnection -> Effect String

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

sendBlob :: WebSocketConnection -> Buffer -> Effect Unit
sendBlob = sendImpl

sendArrayBuffer :: WebSocketConnection -> ArrayBuffer -> Effect Unit
sendArrayBuffer = sendImpl

sendArrayBufferView :: forall t. WebSocketConnection -> ArrayView t -> Effect Unit
sendArrayBufferView = sendImpl

foreign import sendImpl :: forall a. WebSocketConnection -> a -> Effect Unit

foreign import onMessage_ ::
  EffectFn2
    WebSocketConnection
    (EffectFn1 Foreign Unit)
    Unit

-- | Attaches a message event handler to a WebSocketConnection
onMessage ::
  WebSocketConnection ->
  (WebSocketMessage -> Effect Unit) ->
  Effect Unit
onMessage ws callback = runEffectFn2 onMessage_ ws (mkEffectFn1 cb)
  where
  cb fd = case typeOf fd of
    "string" -> callback (WebSocketStringMessage $ unsafeFromForeign fd)
    _ -> do
      bt <- getBinaryType ws
      case bt of
        Blob -> callback (WebSocketBinaryBufferMessage $ unsafeFromForeign fd)
        ArrayBuffer -> callback (WebSocketBinaryArrayBufferMessage $ unsafeFromForeign fd)

foreign import onStringMessage_ ::
  EffectFn2
    WebSocketConnection
    (EffectFn1 String Unit)
    Unit

-- | Attaches a string message event handler to a WebSocketConnection
-- | Convenience function when you known you don't need to handle binary messages.
onStringMessage ::
  WebSocketConnection ->
  (String -> Effect Unit) ->
  Effect Unit
onStringMessage ws callback = runEffectFn2 onStringMessage_ ws (mkEffectFn1 callback)

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

-- foreign import sendMessage_ ::
--   EffectFn2
--     WebSocketConnection
--     WebSocketMessage
--     Unit
-- -- | Send a message over a WebSocketConnection
-- sendMessage ::
--   WebSocketConnection ->
--   WebSocketMessage ->
--   Effect Unit
-- sendMessage ws message = runEffectFn2 sendMessage_ ws message
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
