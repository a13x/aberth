-module(aberth_handler).

-export([init/4]).

init(_Ref, Transport, Socket, _Opts) ->
    wait_request(Transport, Socket).

wait_request(Transport, Socket) ->
    case  Transport:recv(Socket, 0, 30000) of
        {ok, BinaryData} ->
            <<_Len:32, Payload/binary>> = BinaryData,
            Term = bert:decode(Payload),
            io:format("term ~p~n", [Term]),
            %% lets just throw an error
            Returned = bert:encode({error, [server, 2, <<"ServerError">>, <<"Sorry">>, []]}),
            RetSize = byte_size(Returned),
            Transport:send(Socket, << RetSize:32, Returned/binary >>),
            wait_request(Transport, Socket);
        {error, _Reason} ->
            Transport:close(Socket)
    end.