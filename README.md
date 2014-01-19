# aberth - generic BERT-RPC server written in Erlang

Copyright (c) 2014 Aleksandar Radulovic.

__Version:__ 0.9


aberth is a **generic BERT-RPC server** in Erlang. It uses
[barrel](https://github.com/benoitc/barrel) for as TCP acceptor pool
which provides low-latency when accepting connections.

## Supported BERT-RPC features

Aberth supports call, cast and info BERPs. It is up to you, however, to make use of info packets.

### TODO

There's still a bit of thinking left on how to pass the info BERPs to the handler modules in a neat way.
Therefore, at the moment, those are not passed through - if you have an idea on how to elegantly solve this,
feel free to send a pull request!

## Usage

### Create a simple module (or two)

```
-module(some_module).

-export([some_method/3]).

some_method(One, Two, Three) ->
	dict:from_list([{one, One}, {two, Two}, {three, Three}]).
```

```
-module(some_adder).

-export([add/2]).

add(One, Two) ->
	One + Two.
```

### Start aberth server

```
NumberOfAcceptors = 100,
Port = 10001,
Handlers = [some_module, some_adder],
aberth:start_server(NumberOfAcceptors, Port, Handlers).
```

That's about it - aberth is an OTP app so you can easily make it a part of your own OTP app.

### Test the server (from python)

```
import bertrpc
service = bertrpc.Service('localhost', 10001)
response = service.request('call').some_adder.add(203,302)
assert response == 505
```

### Test the server (from Ruby)

```
require 'bertrpc'
svc = BERTRPC::Service.new('localhost', 10001)
r = svc.call.bubu.add(1234, 4321)
puts r
```



