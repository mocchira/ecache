-module(ecache_server).
-behaviour(gen_server).
-vsn("0.0.1").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/2, stop/1, get/1, get/2, set/2, set/3, delete/1, delete/2, stats/1, stats/0]).

% records
-record(value, {
	data		= <<>> :: binary(),
	timestamp	= 0    :: integer()
}).

-record(state, {
	gb_timestamp	= []:: gb_tree(),
	rec_max_size	= 0 :: integer(),
	stats_get_op	= 0 :: integer(),
	stats_set_op	= 0 :: integer(),
	stats_del_op	= 0 :: integer(),
	stats_rec_num	= 0 :: integer(),
	stats_rec_size	= 0 :: integer()
}).

% ============================ API ============================================
% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the server.
start_link(Id, Options) ->
	gen_server:start_link({local, Id}, ?MODULE, [Options], []).

% Function: -> ok
% Description: Manually stops the server.
stop(Id) ->
	gen_server:cast(Id, stop).

% Function: -> binary() 
% Description: get a value associated with a specified key
get(Id, Key) ->
	gen_server:call(Id, {get, Key}).

get(Key) ->
	Id = ecache_sup:get_server_id(Key),
	gen_server:call(Id, {get, Key}).
% Function -> ok
% Description: set a key-value pair
set(Id, Key, Value) when is_list(Value) ->
	NewVal = list_to_binary(Value),
	set(Id, Key, NewVal);
set(Id, Key, Value) when is_atom(Value) ->
	NewVal = atom_to_binary(Value, latin1),
	set(Id, Key, NewVal);
set(Id, Key, Value) when is_binary(Value) ->
	gen_server:call(Id, {set, Key, Value}).

set(Key, Value) ->
	Id = ecache_sup:get_server_id(Key),
	set(Id, Key, Value).
% Function -> ok
% Description: delete a key-value pair by a specified key
delete(Id, Key) ->
	gen_server:call(Id, {delete, Key}).

delete(Key) ->
	Id = ecache_sup:get_server_id(Key),
	gen_server:call(Id, {delete, Key}).
% Function -> any()
% Description: return server's State
stats(Id) ->
	gen_server:call(Id, stats).

stats() ->
	ecache_sup:server_ids_foldl(
		fun(Id, _Acc=#state{
				stats_get_op = GetOp,
				stats_set_op = SetOp,
				stats_del_op = DelOp,
				stats_rec_num = RecNum,
				stats_rec_size = RecSize}) ->
			NewStats = stats(Id),
			#state{
				stats_get_op = GetOp + NewStats#state.stats_get_op,
				stats_set_op = SetOp + NewStats#state.stats_set_op,
				stats_del_op = DelOp + NewStats#state.stats_del_op,
				stats_rec_num = RecNum + NewStats#state.stats_rec_num,
				stats_rec_size = RecSize + NewStats#state.stats_rec_size}
		end, #state{}).
% ============================ GEN_SERVER CALLBACKS ===========================
init([Options]) ->
	% process_flag(sensitive, true),
	OptionProps = [
		% socket
		{rec_max_size, 1024 * 1024 * 1024, fun is_integer/1, rec_max_size_not_integer}
	],
	OptionsVerified = lists:foldl(fun(OptionName, Acc) -> [get_option(OptionName, Options)|Acc] end, [], OptionProps),
	case proplists:get_value(error, OptionsVerified) of
		undefined ->
			% ok, no error found in options
			MaxSize = proplists:get_value(rec_max_size, OptionsVerified),
			{ok, #state{gb_timestamp = gb_trees:empty(), rec_max_size = MaxSize}};
		Reason ->
			% error found in options
			{stop, Reason}
	end.

% handle_call generic fallback
handle_call({get, Key}, _From, State=#state{gb_timestamp = Gbt, stats_get_op = GetOp}) ->
	Ts = timestamp(),
	NewGetOp = GetOp + 1,
	Val = erlang:get(Key),
	case Val of
		undefined ->
			{reply, undefined, State#state{stats_get_op = NewGetOp}};
		_Exist ->
			erlang:put(Key, #value{data = Val#value.data, timestamp = Ts}),
			Gbt2 = gb_trees:delete(Val#value.timestamp, Gbt),
			Gbt3 = gb_trees:insert(Ts, Key, Gbt2),
			{reply, Val#value.data, State#state{gb_timestamp = Gbt3, stats_get_op = NewGetOp}}
	end;

% handle_call generic fallback
handle_call({set, Key, Value}, _From, 
		State=#state{rec_max_size = RecMax, gb_timestamp = Gbt, stats_set_op = SetOp, stats_rec_num = RecNum, stats_rec_size = RecSize}) ->
	Ts = timestamp(),
	Old = erlang:put(Key, #value{data = Value, timestamp = Ts}),
	{NewGbt, NewRecNum, NewRecSize} = case Old of
		undefined ->
			{gb_trees:insert(Ts, Key, Gbt), RecNum + 1, RecSize + size(Value)};
		_Exist ->
			Gbt2 = gb_trees:delete(Old#value.timestamp, Gbt),
			{gb_trees:insert(Ts, Key, Gbt2), RecNum, RecSize + size(Value) - size(Old#value.data)}
	end,
	{NewGbt2, NewRecNum2, NewRecSize2} = cleanup_expired(RecMax, NewRecNum, NewRecSize, NewGbt),
	NewSetOp = SetOp + 1,
	{reply, Old, State#state{gb_timestamp = NewGbt2, stats_set_op = NewSetOp, stats_rec_num = NewRecNum2, stats_rec_size = NewRecSize2}};

% handle_call generic fallback
handle_call({delete, Key}, _From, 
		State=#state{gb_timestamp = Gbt, stats_del_op = DelOp, stats_rec_num = RecNum, stats_rec_size = RecSize}) ->
	Old = erlang:erase(Key),
	{NewGbt, NewRecNum, NewRecSize} = case Old of
		undefined ->
			{Gbt, RecNum, RecSize};
		_Exist ->
			{gb_trees:delete(Old#value.timestamp, Gbt), RecNum - 1, RecSize - size(Old#value.data)}
	end,
	NewDelOp = DelOp + 1,
	{reply, Old, State#state{gb_timestamp = NewGbt, stats_del_op = NewDelOp, stats_rec_num = NewRecNum, stats_rec_size = NewRecSize}};

% handle_call generic fallback
handle_call(stats, _From, State) ->
	{reply, State, State};

% handle_call generic fallback
handle_call(_Request, _From, State) ->
	{reply, undefined, State}.

% manual shutdown
handle_cast(stop, State) ->
	{stop, normal, State};

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
	{noreply, State}.

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: terminate(Reason, State) -> void()
% Description: This function is called by a gen_server when it is about to terminate. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
% ----------------------------------------------------------------------------------------------------------
terminate(_Reason, _State) ->
	terminated.

% ----------------------------------------------------------------------------------------------------------
% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
% Description: Convert process state when code is changed.
% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ============================ INTERNAL FUNCTIONS ==============================
% Description: Validate and get misultin options.
cleanup_expired(RecMax, RecNum, RecSize, Gbt) when RecSize < RecMax ->
	{Gbt, RecNum, RecSize};
cleanup_expired(RecMax, RecNum, RecSize, Gbt) ->
	{_Ts, Key} = gb_trees:smallest(Gbt),
	Old = erlang:erase(Key),
	{NewGbt, NewRecNum, NewRecSize} = case Old of
		undefined ->
			{Gbt, RecNum, RecSize};
		_Exist ->
			{gb_trees:delete(Old#value.timestamp, Gbt), RecNum - 1, RecSize - size(Old#value.data)}
	end,
	cleanup_expired(RecMax, NewRecNum, NewRecSize, NewGbt).

get_option({OptionName, DefaultValue, CheckAndConvertFun, FailTypeError}, Options) ->
	case proplists:get_value(OptionName, Options) of
		undefined ->
			case DefaultValue of
				{error, Reason} ->
					{error, Reason};
				Value -> 
					{OptionName, Value}
			end;
		Value ->
			case CheckAndConvertFun(Value) of
				false ->
					{error, {FailTypeError, Value}};
				true -> 
					{OptionName, Value};
				OutValue ->
					{OptionName, OutValue}
			end
	end.

timestamp() ->
	{Mega,Sec,Micro} = erlang:now(),
	(Mega*1000000+Sec)*1000000+Micro.
