#!/bin/bash

err() {
  echo "[$(date +'%Y-%m-%dT%H:%M:%S%z')]: $@" >&2
  exit 1
}

cd "$(dirname "$0")"/.. || err "can't cd to project dir"

PORT=8000

BIN=_build/install/default/bin/
CLIENT=$BIN/test_viewservice
SERVER=$BIN/viewservice

$SERVER -p $PORT & 
SERVER_ID=$!
sleep 0.1
if ! ps -p $SERVER_ID > /dev/null; then 
  err "error launching server" 
fi

if ! $CLIENT -p $PORT ; then 
   kill $SERVER_ID || err "can't kill server"
   echo "killed server"
   err "client terminate with error"
fi


kill $SERVER_ID || err "can't kill server"
