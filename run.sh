#!/usr/bin/env bash

usage() {
  echo "Usage: $0 [--warnings]"
}

if [ "$#" -gt 1 ]; then
  usage
  exit 1
fi

if [ "$#" -eq 1 ] && [ "$1" = "--warnings" ]; then
  dune exec symbolic_execution
else
  dune exec symbolic_execution --profile release
fi


exit 0
