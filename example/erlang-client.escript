#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz ../ebin ../deps/*/ebin



main(_) ->
    Res = aberth:call("localhost",10001,{mfa, example, adder, [2400,400]}, []),
    io:format("~p~n", [Res]).
