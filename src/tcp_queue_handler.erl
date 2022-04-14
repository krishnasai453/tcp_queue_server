-module(tcp_queue_handler).

-behaviour(gen_server).

%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("tcp_queue.hrl").

%%%===================================================================
%%% External API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the queue handler gen server
%% - `ListenSocket`: The socket_connection pid
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid()) -> {ok, pid()} | {error, term()} | ignore.
start_link(ListenSocket) ->
    gen_server:start_link(?MODULE, ListenSocket, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Initializes the server and in memory state
-spec init(pid()) ->
              {ok, #state{}} |
              {ok, #state{}, timeout() | hibernate} |
              {ok, #state{}, {continue, term()}} |
              {stop, term()} |
              ignore.
init(ListenSocket) ->
    gen_server:cast(self(), accept_once),
    {ok, #state{listen_socket = ListenSocket}}.

%% gen_server callback for handle call messages
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% gen_server callback for handle cast messages
-spec handle_cast(Request :: term(), State :: #state{}) ->
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(accept_once, #state{listen_socket = ListenSocket} = State) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    inet:setopts(AcceptSocket, [{active, once}]),
    {noreply, State#state{accept_socket = AcceptSocket}}.

%% gen_server callback for handle info messages
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_info(ReceivedMessage, State) ->
    io:format("=======ReceivedMessage is ~p ~n", [ReceivedMessage]),
    handle_incomming_data(ReceivedMessage, State).

%% terminate gen_server callback is invoked by a gen_server process when it is about to terminate
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: #state{}) ->
                   term().
terminate(_Reason, _State) ->
    ok.

%% code_change gen_server callback is invoked when it is to update its internal
%% state during a release upgrade/downgrade code
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% handling various incomming messages from socket
handle_incomming_data({tcp, _AcceptSocket, <<"quit", _/binary>>}, State) ->
    handle_quit_message(State);
handle_incomming_data({tcp, _AcceptSocket, <<"out", _/binary>>}, State) ->
    handle_out_message(State);
handle_incomming_data({tcp, _AcceptSocket, <<"in ", Item/binary>>}, State) ->
    handle_in_message(Item, State);
handle_incomming_data({tcp, _AcceptSocket, <<"in", Item/binary>>}, State) ->
    handle_in_message(Item, State);
handle_incomming_data({tcp, _AcceptSocket, _Msg}, State) ->
    io:format("received unsupported message ~n"),
    handle_no_match(State);
%Connection closed from client
handle_incomming_data({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_incomming_data({tcp_error, _Socket, _}, State) ->
    {stop, normal, State};
handle_incomming_data(Error, State) ->
    io:format("Error handling incomming data ~p ~n", [Error]),
    {noreply, State}.

%% sending back response to client
send(AcceptSocket, Message) ->
    gen_tcp:send(AcceptSocket, Message),
    inet:setopts(AcceptSocket, [{active, once}]).

%% handles quit command message
handle_quit_message(#state{accept_socket = AcceptSocket} = State) ->
    close_connection(AcceptSocket),
    {stop, normal, State}.

%% closes tcp connection
close_connection(AcceptSocket) ->
    send(AcceptSocket, "Closing Connection.\n"),
    gen_tcp:close(AcceptSocket).

%% handles out command messages
handle_out_message(#state{queue = Queue} = State) ->
    handle_out_message(tcp_queue:out(Queue), State).

handle_out_message({{value, Item}, RestQueue},
                   #state{accept_socket = AcceptSocket} = State) ->
    send(AcceptSocket, io_lib:format("Out: ~s~n", [Item])),
    {noreply, State#state{queue = RestQueue}};
handle_out_message(empty, #state{accept_socket = AcceptSocket} = State) ->
    send(AcceptSocket, "Queue is empty.\n"),
    {noreply, State}.

%% handles in command messages
handle_in_message(Item, #state{queue = Queue, accept_socket = AcceptSocket} = State) ->
    NewQueue = tcp_queue:in(Item, Queue),
    send(AcceptSocket, "Added to queue.\n"),
    {noreply, State#state{queue = NewQueue}}.

%% handles unsupported commands
handle_no_match(#state{accept_socket = AcceptSocket} = State) ->
    send(AcceptSocket, "Not supported.\n"),
    {noreply, State}.
