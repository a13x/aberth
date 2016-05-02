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

-module(aberth_protocol).
-behaviour(ranch_protocol).

-type bert_type() :: call | cast.
-type bert_info() :: any().
-record(state, {
        transport = undefined,
        socket = undefined,
        type :: bert_type(),
        info :: bert_info()
    }).
-type state() :: #state{}.

-export([start_link/4, init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{packet, 4}]),
    {ok, {Ip, _Port}} = inet:peername(Socket),
    lager:info("got connection from ~p", [inet_parse:ntoa(Ip)]),
    wait_request(#state{transport = Transport, socket = Socket}).

-spec wait_request(state()) -> ok.
wait_request(State=#state{transport=Transport, socket=Socket}) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Payload} ->
            Term = bert:decode(Payload),
            case process(Term, State) of
                {ok, Reply} -> reply(Reply, State);
                {info, Info} -> wait_request(State#state{info=Info})
            end;
        {error, _Reason} ->
            Transport:close(Socket)
    end.

reply(Reply, State=#state{transport=Transport, socket=Socket}) ->
    lager:debug("will return ~p~n", [Reply]),
    Returned = bert:encode(Reply),
    Transport:send(Socket, Returned),
    wait_request(State).

process_module(Mod, Fun, Args, Info) ->
    case lists:member(Mod, erlang:loaded()) of
        true -> process_method(Mod, Fun, Args, Info);
        false -> aberth:not_loaded(Mod)
    end.

process_method(Mod, Fun, Args, _Info) ->
    case erlang:function_exported(Mod, Fun, length(Args)) of
        true -> {reply, apply(Mod, Fun, Args)};
        false -> aberth:not_allowed(Fun)
    end.

process_call(Mod, Fun, Args, Info) ->
    case aberth_server:allowed(Mod) of
        true -> process_module(Mod, Fun, Args, Info);
        false -> aberth:no_such_module(Mod)
    end.

process({call, Mod, Fun, Args}, _State=#state{info=Info}) ->
    lager:debug("info packet is ~p~n", [Info]),
    Reply = process_call(Mod, Fun, Args, Info),
    {ok, Reply};

process({cast, Mod, Fun, Args}, _State=#state{info=Info}) ->
    spawn(fun() -> process_call(Mod, Fun, Args, Info) end),
    {ok, {noreply}};

process({info, Command, Options}, _State) ->
    lager:debug("info command: ~p~n", [Command]),
    lager:debug("info options: ~p~n", [Options]),
    {info, {Command, Options}};

process(_, _) ->
    {ok, {error,
        {protocol, 2, <<"ProtocolError">>, [],[]}
    }}.
