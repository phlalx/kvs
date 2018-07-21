#!/bin/bash

err() {
  echo "[$(date +'%Y-%m-%dT%H:%M:%S%z')]: $@" >&2
  exit 1
}

cd "$(dirname "$0")"/.. || err "can't cd to project dir"

PORT=8000

BIN=_build/install/default/bin/
CLIENT=$BIN/test_viewservice

$CLIENT -p $PORT
