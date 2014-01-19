-module(aberth_protocol).

-record(state, {
        transport = undefined,
        socket = undefined,
        module = undefined,
        function = undefined,
        args = [],
        info = undefined
    }).

-export([init/4]).

init(_Ref, Transport, Socket, _Opts) ->
    wait_request(#state{transport = Transport, socket = Socket}).

wait_request(State=#state{transport=Transport, socket=Socket}) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Payload} ->
            Term = bert:decode(Payload),
            io:format("term ~p~n", [Term]),
            case process(Term, State) of
                {ok, Reply} -> reply(Reply, State);
                {info, Info} -> wait_request(State#state{info=Info})
            end;
        {error, _Reason} ->
            Transport:close(Socket)
    end.

reply(Reply, State=#state{transport=Transport, socket=Socket}) ->
    io:format("return ~p~n", [Reply]),
    Returned = bert:encode(Reply),
    Transport:send(Socket, Returned),
    wait_request(State).

call(Mod, Fun, Args) ->
    case aberth_server:allowed(Mod) of
        % true -> Reply = dict:from_list([{bleh, <<"blah">>}, {boo, <<"BOOB">>}, {what, 134}]);
        true -> {reply, Mod:Fun(Args)};
        false -> aberth:no_such_module(Mod)
    end.

process({call, Mod, Fun, Args}, _State=#state{info=Info}) ->
    io:format("info ~p~n", [Info]),
    io:format("module ~p~n", [Mod]),
    Reply = call(Mod, Fun, Args),
    {ok, Reply};

process({cast, _Mod, _Fun, _Args}, _State) ->
    {ok, {noreply}};

process({info, Command, Options}, _State) ->
    io:format("info command: ~p~n", [Command]),
    io:format("info options: ~p~n", [Options]),
    {info, {Command, Options}};

process(_, _) ->
    {ok, {error,
        {protocol, 2, <<"ProtocolError">>, [],[]}
    }}.
