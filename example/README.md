# black_adder - example aberth service app

## Requisites

#### Server

To run the Aberth server you will need [rebar](https://github.com/rebar/rebar)
to get dependencies and compile, and of course [erlang](https://erlang.org) to
run it.

#### Examples

Each example has different requisites based on the platform:

* `python` requires `pip`
* `ruby` requires `bundler`
* `node` requires `npm` or `yarn`

## Getting Started

#### Server

Jump into the `server` folder and lets compile and start our Aberth server:

    $ cd server
    $ rebar get-deps compile
    $ ./start.sh

Now you can call the service with any BERT-RPC clients, like we show in the
examples clients in here.

#### Clients

After starting the server, you can jump into any client folder, install
dependencies, and run the sample code.

##### Node

    $ cd client-node
    $ yarn
    $ node index.js

##### Ruby

    $ cd client-ruby
    $ bundle
    $ ruby bertrpc.rb

##### Python

    $ cd client-python
    $ pip install -r requirements.txt
    $ python bertrpc.py

## Credits

Copyright (c) 2014 Aleksandar Radulovic
