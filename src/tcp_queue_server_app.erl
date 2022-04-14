%%%-------------------------------------------------------------------
%% @doc tcp_queue_server public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_queue_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%% This function is called whenever application is started.
%% It triggers the start of supervisor
-spec start(StartType :: normal | {takeover, node()} | {failover, node()},
            StartArgs :: term()) ->
               {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    tcp_queue_server_sup:start_link().

%% This function is called whenever application is stopped.
-spec stop(State :: term()) -> term().
stop(_State) ->
    ok.
