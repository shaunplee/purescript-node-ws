"use strict";

const WebSocket = require('ws');

/* WebSocket methods */
exports.createWebSocketConnection_ = function(url, protocols, options) {
  return new WebSocket(url, protocols, options);
};

exports.url = function (ws) {
  return function () {
    return ws.url;
  };
};

exports.readyStateImpl = function (ws) {
  return function () {
    return ws.readyState;
  };
};

exports.bufferedAmount = function (ws) {
  return function () {
    return ws.bufferedAmount;
  };
};

exports.extensions = function (ws) {
  return function () {
    return ws.extensions;
  };
};

exports.protocol = function (ws) {
  return function () {
    return ws.protocol;
  };
};

exports.getBinaryTypeImpl = function (ws) {
  return function () {
    return ws.binaryType;
  };
};

exports.setBinaryTypeImpl = function (ws) {
  return function (bt) {
    return function () {
      ws.binaryType = bt;
    };
  };
};

exports.sendImpl = function (ws) {
  return function (value) {
    return function () {
      ws.send(value);
    };
  };
};

exports.onMessage_ = function(ws, handleMessage) {
  ws.on('message', handleMessage);
};

exports.onStringMessage_ = function(ws, handleMessage) {
  ws.on('message', handleMessage);
};

exports.onOpen_ = function(ws, handleOpen) {
  ws.on('open', handleOpen);
};

exports.onClose_ = function(ws, handleClose) {
  ws.on('close', handleClose);
};

exports.onError_ = function(ws, handleError) {
  ws.on('error', handleError);
};

// exports.sendMessage_ = function(ws, message) {
//   ws.send(message);
// };

exports.close_ = function(ws, code, reason) {
  ws.close(code, reason);
};
