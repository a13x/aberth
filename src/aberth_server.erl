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

-module(aberth_server).
-behaviour(gen_server).
-author("Aleksandar Radulovic <alex@a13x.net>").

-export([start/1, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([allowed/1]).

-spec start(aberth:handlers()) -> {ok, pid()} | {error, any()}.
start(Handlers) ->
	gen_server:start({local, ?MODULE}, ?MODULE, Handlers, []).

-spec start_link(aberth:handlers()) -> {ok, pid()} | {error, any()}.
start_link(Handlers) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Handlers, []).

-spec allowed(aberth:handler()) -> true | false.
allowed(Handler) ->
	gen_server:call(?MODULE, {lookup, Handler}).

%% gen_server API
init(Handlers) ->
	ets:new(?MODULE, [set, named_table, protected]),
	true = ets:insert(?MODULE, {handlers, Handlers}),
	{ok, ?MODULE}.

handle_call({lookup, Handler}, _From, Table) ->
	[{handlers, Handlers}] = ets:lookup(Table, handlers),
	io:format("I have handlers: ~p~n", [Handlers]),
	io:format("Looking for handler: ~p~n", [Handler]),
	Reply = lists:member(Handler, Handlers),
	io:format("reply is: ~p~n", [Reply]),
	{reply, Reply, Table};

handle_call(_Msg, _From, Table) ->
    {noreply, Table}.

handle_cast(stop, Table) ->
    {stop, normal, Table};

handle_cast(_Msg, Table) ->
    {noreply, Table}.

handle_info(_Msg, Table) ->
    {noreply, Table}.

terminate(normal, _Table) ->
    ok.

code_change(_OldVsn, Table, _Extra) ->
    {ok, Table}.