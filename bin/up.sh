#!/bin/bash

echo "Erlang nodes"
cat .hosts.erlang

rebar3 as test shell --name reqsrv --setcookie reqsrv
