-module(tcp_queue).%% Module to handle queue datastructure properties

%% API
-export([new/0]).
-export([in/2, out/1]).

%% Creates new queue(tuple with empty lists)
-spec new() -> tuple().
new() ->
    {[], []}.

%% inserts items to queue
-spec in(binary(), tuple()) -> tuple().
in(Item, {First, Last}) ->
    {First, [Item | Last]}.

%% removes items to queue
-spec out(tuple()) -> {{value, binary()}, tuple()} | empty.
out({[], []}) ->
    empty;
out({[H | T], Back}) ->
    {{value, H}, {T, Back}};
out({[], Back}) ->
    out({lists:reverse(Back), []}).
