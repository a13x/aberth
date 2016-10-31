#!/bin/sh
##
# ./start-dev-sh -black_adder port 10001
##
erl -pa ebin ../deps/*/ebin ../ebin -s black_adder "$@"
