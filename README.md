# purescript-node-ws

Low-level PureScript bindings for [`ws`](https://github.com/websockets/ws) library.
Currently only contains WebSocket server functionality and basic client functionality
patterned after
[purescript-web-socket](https://github.com/purescript-web/purescript-web-socket).
Like `ws`, this library is to be used on Node.js, not in the browser.

Sample usage in [example/](/example/) directory.

NOTE: You **MUST** attach an error handler for each `WebSocketConnection` by
using `onError`, otherwise your program will crash and burn on any error that
occurs with the `WebSocketConnection` (such as ECONNRESET from a client that
abruptly closed its end of the connection). This behavior is caused by the `ws`
library.

## Setup

1. Install `ws` from npm.
2. Have a look at the tests and the examples

## Testing

Test has a few dependencies not used by the mail library, so these
dependencies appear in a separate `test.dhall` file. Run the tests with:

```
spago -x test.dhall test
```

The tests assume that ports 9000-9003 are available for running local servers.

## TODO

- Force user to supply `WebSocketConnection` event handlers in
  `createWebSocketServer*` functions
