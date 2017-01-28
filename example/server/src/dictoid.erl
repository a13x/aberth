-module(dictoid).

-export([to_dict/3]).

to_dict(One, Two, Three) ->
    dict:from_list([{one, One}, {two, Two}, {three, Three}]).
