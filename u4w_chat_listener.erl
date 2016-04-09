%%%-------------------------------------------------------------------
%%% @author  <rfjacob@RFJACOB-39DA957>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 27 Feb 2016 by  <rfjacob@RFJACOB-39DA957>
%%%-------------------------------------------------------------------
-module(u4w_chat_listener).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_loop() ->
    gen_server:cast(?SERVER, start_loop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ssl:start(),
    Port = element(2, application:get_env(u4w_chat, port)),
    {ok, ListenSocket} = 
	ssl:listen(Port, 
		   [{certfile, element(2, application:get_env(u4w_chat, certfile))},%"../priv/cert.pem" 
		    {keyfile, element(2, application:get_env(u4w_chat, keyfile))},%"../priv/key.pem"
		    {reuseaddr, true},
		    {active, true},
		    {password, element(2, application:get_env(u4w_chat, password))}]),%"12345678"
    u4w_chat_log:log("Server has started listenning on port: " 
		++ integer_to_list(Port)),
    start_loop(),

    {ok, ListenSocket}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(shutdown, State) ->
    u4w_chat_log:log("Server is shutting down ..."),
    {stop, {shutdown, server_expired}, State};
handle_cast(_Msg, State) ->
    server_loop(State),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

server_loop(ServerSocket) ->
    {ok, Socket} = ssl:transport_accept(ServerSocket),
    {sslsocket,_, %%{gen_tcp, _ ,tls_connection, _}, 
     SocketPID} = Socket,
    ActiveSessionName=list_to_atom(atom_to_list(?SERVER) 
				   ++ pid_to_list(SocketPID)),

    echat_sup:start_session(ActiveSessionName, echat_session_handler, [Socket]),
    u4w_chat_log:log("A new connection attempt is detected. " ++ atom_to_list(ActiveSessionName)),
    server_loop(ServerSocket).
    
