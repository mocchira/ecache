-module(ecache_server).
-behaviour(gen_server).
-vsn("0.0.1").

-include("ecache.hrl").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/2, stop/1, get/1, get/2, set/2, set/3, delete/1, delete/2, stats/1, stats/0]).

-record(state, {
	cherly          = <<>>          ,
	rec_max_size	= 0 :: integer(),
	stats_get_op	= 0 :: integer(),
	stats_set_op	= 0 :: integer(),
	stats_del_op	= 0 :: integer(),
	stats_hit       = 0 :: integer()
}).

% ============================ API ============================================
% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the server.
start_link(Id, Options) ->
	gen_server:start_link({local, Id}, ?MODULE, Options, []).

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
	case size(Value) < 1000000 of
		true ->
			gen_server:call(Id, {set, Key, Value});
		false ->
			nop
	end.

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
	lists:foldl(
		fun(Id, _Acc=#stats{
				get_op = GetOp,
				set_op = SetOp,
				del_op = DelOp,
				hit_cnt = HitCount,
				rec_num = RecNum,
				rec_size = RecSize}) ->
			NewStats = stats(Id),
			#stats{
				get_op = GetOp + NewStats#stats.get_op,
				set_op = SetOp + NewStats#stats.set_op,
				del_op = DelOp + NewStats#stats.del_op,
				hit_cnt = HitCount + NewStats#stats.hit_cnt,
				rec_num = RecNum + NewStats#stats.rec_num,
				rec_size = RecSize + NewStats#stats.rec_size}
		end, #stats{}, ecache_sup:get_server_ids()).
% ============================ GEN_SERVER CALLBACKS ===========================
init([MaxSize|_T]) ->
	io:format("max:~p~n",[MaxSize]),
	Cherly = cherly:start(MaxSize),
	{ok, #state{
		rec_max_size = MaxSize,
		cherly       = Cherly
	}}.

% handle_call generic fallback
handle_call({get, Key}, _From, State=#state{cherly = Cherly, stats_get_op = GetOp, stats_hit = HitCount}) ->
	NewGetOp = GetOp + 1,
	Val = cherly:get(Cherly, Key),
	case Val of
		none ->
			{reply, undefined, State#state{stats_get_op = NewGetOp}};
		_Exist ->
			{reply, Val, State#state{stats_get_op = NewGetOp, stats_hit = HitCount + 1}}
	end;

% handle_call generic fallback
handle_call({set, Key, Val}, _From, 
		State=#state{cherly = Cherly, stats_set_op = SetOp}) ->
	Ret = cherly:put(Cherly, Key, Val),
	NewSetOp = SetOp + 1,
	{reply, Ret, State#state{stats_set_op = NewSetOp}};

% handle_call generic fallback
handle_call({delete, Key}, _From, 
		State=#state{cherly = Cherly, stats_del_op = DelOp}) ->
	Ret = cherly:remove(Cherly, Key),
	NewDelOp = DelOp + 1,
	{reply, Ret, State#state{stats_del_op = NewDelOp}};

% handle_call generic fallback
handle_call(stats, _From, State=#state{
		cherly = Cherly,
		stats_hit = HitCount,
		stats_get_op = GetOp,
		stats_set_op = SetOp, 
		stats_del_op = DelOp}) ->
	Stats = #stats{
		hit_cnt = HitCount,
		get_op = GetOp,
		set_op = SetOp,
		del_op = DelOp,
		rec_num  = cherly:items(Cherly),
		rec_size = cherly:size(Cherly)},
	{reply, Stats, State};

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

