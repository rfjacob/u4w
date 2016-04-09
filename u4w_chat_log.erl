%%%-------------------------------------------------------------------
%%% @author  <rfjacob@RFJACOB-39DA957>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 27 Feb 2016 by  <rfjacob@RFJACOB-39DA957>
%%%-------------------------------------------------------------------
-module(u4w_chat_log).

-behaviour(gen_server).

%% API
-export([start_link/0, log/1]).

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

log(Message) ->
    gen_server:cast(?SERVER, {log, Message}).
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
    {ok, init_state()}. %{filename, file_handler} 

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
handle_cast({log, Message}, State) ->
    {FileName, FH} = log_size_check(State),
    file:write(FH, format(Message)),
    {noreply, {FileName, FH}};
handle_cast(shutdown, State) ->
    {stop, {shutdown, server_expired}, State}.

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
init_state() ->
    FileName = 
	element(2, application:get_env(u4w_chat, log_dir)) ++ "/" ++ 
	element(2, application:get_env(u4w_chat, log_file_prefix)) 
	++ integer_to_list(
	     calendar:datetime_to_gregorian_seconds(
	       calendar:universal_time())),
    {ok, FH} = 
	file:open(FileName, [write]),
    {FileName, FH}.

log_size_check({FileName, FH}) ->
    MaxLogSizeB = 1024 * element(2, application:get_env(u4w_chat, log_wrap_kb)),
    {ok,FileInfo} = file:read_file_info(FileName),
    FileSizeB = element(2, FileInfo),
    if
	FileSizeB < MaxLogSizeB ->
	    {FileName, FH};
	true ->
	    file:close(FH),
	    init_state()
    end.
    
format(Message) ->
    timestamp() ++ " :: " ++ Message ++ "\n".

timestamp() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:universal_time(),
    lists:flatten(
      io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B+00:00", 
		    [Year, Month, Day, Hour, Min, Sec])).

