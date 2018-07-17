#!/bin/bash

err() {
  echo "[$(date +'%Y-%m-%dT%H:%M:%S%z')]: $@" >&2
  exit 1
}

cd "$(dirname "$0")"/.. || err "can't cd to project dir"

PORT=8000

BIN=_build/install/default/bin/
CLIENT=$BIN/rpc_client
SERVER=$BIN/server

$SERVER -p $PORT & 
SERVER_ID=$!
sleep 0.1
if ! ps -p $SERVER_ID > /dev/null; then 
  err "error launching server" 
fi

if ! $CLIENT -p $PORT; then 
   kill $SERVER_ID || err "can't kill server"
   err "can't launch client"
fi
sleep 1

kill $SERVER_ID || err "can't kill server"
