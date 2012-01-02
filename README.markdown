### How to use

    $ # tty 1
    $ cd ebin
    $ erlc ../src/*.erl
    $ escript server.erl

    $ # tty 2
    $ cd ebin
    $ escript client-david.erl

    $ # tty 3
    $ cd ebin
    $ escript client-alice.erl
