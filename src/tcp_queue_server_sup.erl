%%%-------------------------------------------------------------------
%% @doc tcp_queue_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_queue_server_sup).

-behaviour(supervisor).

-export([start_link/0, start_new_worker/0]).
%% Supervisor callback
-export([init/1]).

-define(SERVER, ?MODULE).

-include("tcp_queue.hrl").

%%====================================================================
%% API functions
%%====================================================================
%% Starts the supervisor
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Starts the new worker under supervisor
-spec start_new_worker() -> {ok, Pid :: pid()} | ignore | {error, term()}.
start_new_worker() ->
    supervisor:start_child(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% starting supervisor triggeres this callback.
%% It configures restart strategy, maximum restart frequency and child specifications
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

-spec init(Args :: term()) ->
              {ok,
               {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                             MaxRestarts :: non_neg_integer(),
                             MaxRestartIntensity :: non_neg_integer()},
                [ChildSpec :: supervisor:child_spec()]}} |
              ignore |
              {error, Reason :: term()}.
init([]) ->
    %% Note: Can configure Port in env config file
    Port = 8000,
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, false}, binary]),
    SupFlags = {simple_one_for_one, 60, 3600},
    %% Note: Can configure NumberOfWorkers in env config file
    NumberOfWorkers = 20,
    spawn_link(fun() -> start_workers(NumberOfWorkers) end),
    ChildSpecs =
        [{tcp_queue_handler,
          {tcp_queue_handler, start_link, [ListenSocket]},
          temporary,
          1000,
          worker,
          [tcp_queue_handler]}],
    {ok, {SupFlags, ChildSpecs}}.

start_workers(Number) ->
    [start_new_worker() || _Num <- lists:seq(1, Number)].
