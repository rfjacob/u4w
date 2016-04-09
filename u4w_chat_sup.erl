%%%-------------------------------------------------------------------
%%% @author  <rfjacob@RFJACOB-39DA957>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 27 Feb 2016 by  <rfjacob@RFJACOB-39DA957>
%%%-------------------------------------------------------------------
-module(u4w_chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    Listener = {u4w_chat_listener, {u4w_chat_listener, start_link, []},
		Restart, Shutdown, Type, [u4w_chat_listener]},
    
    Archiver = {u4w_chat_archive, {u4w_chat_archive, start_link, []},
		Restart, Shutdown, Type, [u4w_chat_archive]},
    
    Logger = {u4w_chat_log, {u4w_chat_log, start_link, []},
		Restart, Shutdown, Type, [u4w_chat_log]},

    {ok, {SupFlags, [Logger, Archiver,  Listener]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
