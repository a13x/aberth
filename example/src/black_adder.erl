-module(black_adder).

-export([start/0]).

start() ->
    ok = aberth:start(),
    ok = application:start(black_adder).

