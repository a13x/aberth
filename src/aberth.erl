%% Copyright (c) 2013 Aleksandar Radulovic <alex@a13x.net>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(aberth).

%% API.
-export([start/0, stop/0]).

-export([start_server/3]).
%% Utils
-export([no_such_module/1, not_allowed/1, not_loaded/1]).
%% Client
-export([call/5, cast/5]).


%% Types
-type handler() :: module().
-type handlers() :: [handler()].
-export_type([handlers/0]).

%% Aplication
start() ->
    application:ensure_all_started(aberth),
    lager:info("Aberth started.").

stop() ->
	application:stop(aberth).

%% Starting and loading aberth server
-spec start_server(integer(), integer(), aberth:handlers()) -> {ok, pid()} | {error, term()}.
start_server(NbAcceptors, Port, Handlers) ->
%% @doc start aberth BERT-RPC server
%%
%% ```
%%   NbAcceptors = integer()
%%   Port = integer()
%%   Handlers - any(),
%% '''
%%
%% NbAcceptors is a number of processes that receive connections
%% Port is a port number the server should listen to
%% Handlers is a list of modules that are wired to the server
	_ = lists:map((fun code:ensure_loaded/1), Handlers),
	aberth_server:add_handlers(Handlers),
	barrel:start_listener(aberth, NbAcceptors, barrel_tcp,
          [{packet, 4}, {port, Port}], aberth_protocol, []).

%% Utility funs
no_such_module(Module) ->
	Msg = list_to_binary(io_lib:format("Module '~p' not found", [Module])),
	{error, {server, 1,	<<"ServerError">>, Msg, []}}.

not_allowed(Func) ->
	Msg = list_to_binary(io_lib:format("Method '~p' not allowed", [Func])),
	{error, {server, 2,	<<"ServerError">>, Msg, []}}.	

not_loaded(Mod) ->
	Msg = list_to_binary(io_lib:format("Module '~p' not loaded", [Mod])),
	{error, {server, 1, <<"ServerError">>, Msg, []}}.

%% Client API funs

call(Host, Port, Mod, Fun, Args) ->
    call_1(call, Host, Port, Mod, Fun, Args).

cast(Host, Port, Mod, Fun, Args) ->
    call_1(cast, Host, Port, Mod, Fun, Args).

%% Client internal funs

call_1(Type, Host, Port, Mod, Fun, Args) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}, {active, false}]) of
        {ok, Socket} ->
            call_2(Type, Socket, Mod, Fun, Args);
        Error ->
            Error
    end.

call_2(Type, Socket, Mod, Fun, Args) ->
    Request = bert:encode({Type, Mod, Fun, Args}),
    ok = gen_tcp:send(Socket, Request),
    case gen_tcp:recv(Socket, 0) of
        {ok, Received} ->
            gen_tcp:close(Socket),
            decode(Received);
        {error, Reason} ->
            {error, Reason}
    end.

decode(Data) ->
    case bert:decode(Data) of
        {reply, Reply} ->
            Reply;
        {noreply} ->
            ok;
        {error, Error} ->
            {error, Error};
        Other ->
            {error, {bad_response, Other}}
    end.
