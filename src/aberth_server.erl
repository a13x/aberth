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

-export([start/0, start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create_table/0, allowed/1, add_handlers/1]).

-spec start() -> {ok, pid()} | {error, any()}.
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    Workers = 
        case application:get_env(aberth_server, workers) of
            {ok, WorkerNum} -> WorkerNum;
            undefined -> 100
        end,
    lager:info("Starting ~p workers", [Workers]),
    wpool:start_pool(
      ?MODULE,
      [{workers, Workers},
       {worker, {?MODULE, []}}]).


-spec create_table() -> ok.
create_table() ->
    case ets:info(?MODULE, named_table) of
        true -> exists;
        undefined -> 
            ets:new(?MODULE,
                    [public,
                     set,
                     named_table,
                     {read_concurrency, true}])
    end,
    ok.

-spec allowed(aberth:handler()) -> true | false.
allowed(Handler) ->
	wpool:call(?MODULE, {lookup, Handler}).

-spec add_handlers(aberth:handlers()) -> ok.
add_handlers(Handlers) ->
	wpool:call(?MODULE, {add_handlers, Handlers}).

%% gen_server API
init([]) ->
	{ok, ?MODULE}.

handle_call({add_handlers, Handlers}, _From, Table) ->
	true = ets:insert(?MODULE, {handlers, Handlers}),
	{reply, ok, Table};

handle_call({lookup, Handler}, _From, Table) ->
	[{handlers, Handlers}] = ets:lookup(Table, handlers),
	lager:debug("I have handlers: ~p~n", [Handlers]),
	lager:debug("Looking for handler: ~p~n", [Handler]),
	Reply = lists:member(Handler, Handlers),
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
