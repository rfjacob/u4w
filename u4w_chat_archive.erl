%%%-------------------------------------------------------------------
%%% @author  <rfjacob@RFJACOB-39DA957>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 27 Feb 2016 by  <rfjacob@RFJACOB-39DA957>
%%%-------------------------------------------------------------------
-module(u4w_chat_archive).

-behaviour(gen_server).

%% API
-export([start_link/0, write/4, dump/0, check_dump_time/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(archive, {sender, receiver, channel, message, timestamp}).

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

write(Sender, Receiver, Channel, Message) ->
    gen_server:cast(?SERVER, {Sender, Receiver, Channel, Message}).

dump() ->
    gen_server:cast(?SERVER, dump).

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
    initiate_db(),
    {ok, calendar:universal_time()}.

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
handle_cast({Sender, Receiver, Channel, Message}, State) ->
    check_dump_time(State),
    mnesia:dirty_write(#archive{sender=Sender, receiver=Receiver, 
				channel=Channel, message=Message, 
				timestamp=calendar:universal_time()}),
    {noreply, State};
handle_cast(dump, State) ->
    u4w_chat_log:log("Dumping archive started."),
    file:write_file(archive_filename(State), 
      list_to_binary(
	string:join(
	  lists:map(
	    fun(X) -> 
		    datetime_to_timestamp(X#archive.timestamp) 
			++ "," ++ X#archive.sender ++ "," 
			++ X#archive.receiver ++ "," 
			++ X#archive.channel ++ "," 
			++ X#archive.message end, 
	    ets:tab2list(archive)), "\n"))),
    mnesia:clear_table(archive),
    u4w_chat_log:log("Dump archive done successfully."),
    {noreply, calendar:universal_time()};
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
initiate_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(archive, 
			[{attributes, record_info(fields, archive)},
			 {access_mode, read_write},
			 {type, bag}
			]),
    ok.

check_dump_time(State) ->
    Diff = calendar:datetime_to_gregorian_seconds(
	     calendar:universal_time()) -
	calendar:datetime_to_gregorian_seconds(State),
    Freq = element(2, application:get_env(u4w_chat, dump_freq_hours)) * 3600,
    if 
	Diff >= Freq ->
	    gen_server:cast(?SERVER, dump);
	true ->
	    no_dump
    end.

datetime_to_timestamp({{Year,Month,Day},{Hour,Min,Sec}}) ->
    lists:flatten(
      io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B+00:00", 
		    [Year, Month, Day, Hour, Min, Sec])).

now_to_string() ->
    lists:flatten(io_lib:format("~b_~b_~b", tuple_to_list(now()))).

datetime_to_filename_timestamp({{Year,Month,Day},{Hour,Min,Sec}}) ->
    lists:flatten(
      io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B-~2.10.0B-~2.10.0B-00-00", 
		    [Year, Month, Day, Hour, Min, Sec])).

archive_filename(State) ->
    "archive_" ++ datetime_to_filename_timestamp(State) ++ "_" ++ now_to_string().
