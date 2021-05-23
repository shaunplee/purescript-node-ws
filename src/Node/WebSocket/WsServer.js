"use strict";

const WebSocket = require('ws');

/* Server methods */
exports.createWebSocketServer_ = function (options, callback) {
  return new WebSocket.Server(options, callback);
};

exports.onConnection_ = function(wss, handleConnection) {
  wss.on('connection', handleConnection);
};

exports.onServerError_ = function(wss, handleError) {
  wss.on('error', handleError);
};

exports.onServerClose_ = function(wss, callback) {
  wss.on('close', callback);
};

exports.onListening_ = function(wss, callback) {
  wss.on('listening', callback);
};

exports.onHeaders_ = function(wss, callback) {
  wss.on('headers', callback);
};

exports.closeServer_ = function(wss, callback) {
  wss.close(callback);
};
