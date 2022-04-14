tcp_queue_server
=====

An OTP application that acts as tcp queue server accepting input data from socket 

Build
-----

    $ rebar3 compile
    $ rebar3 shell


Client Usage
-----
    telnet localhost 8000
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.

    Input the data into queue
    in <any message> 
    $ in apple
    $ Added to queue.


    Output the data from queue
    $ out
    $ apple
