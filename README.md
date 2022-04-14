tcp_queue_server
=====

An OTP application that acts as tcp queue server accepting input data from socket 

Build
-----

    $ rebar3 compile
    $ rebar3 shell


Client Usage
-----
Input the data into queue
in <any message> 
    $ in apple
    $ Added to queue.

Output the data from queue
    $ out
    $ apple
