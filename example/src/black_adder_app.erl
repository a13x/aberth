-module(black_adder_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% lager:set_loglevel(lager_console_backend, debug),
    {ok, Port} = application:get_env(black_adder, port),
    Handlers = [dictoid, example],
    NbAcceptors = 100,
    aberth:start_server(NbAcceptors, Port, Handlers),
    black_adder_sup:start_link().

stop(_State) ->
    ok.
