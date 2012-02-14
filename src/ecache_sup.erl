-module(ecache_sup).
-vsn('0.0.1').
-behaviour(supervisor).

%% External API
-export([start_link/1, stop/0, get_server_id/1, get_server_ids/0, get_server_ids/1]).

%% Callbacks
-export([init/1]).

-define(SHUTDOWN_WAITING_TIME,          2000).
-define(MAX_RESTART,                       5).
-define(MAX_TIME,                         60).
-define(SERVER_NAME_PREFIX, "ecache_server_").

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @spec (Params) -> ok
%% @doc start link.
%% @end
start_link(Options) ->
	OptionProps = [
                {proc_num, 8, fun is_integer/1, proc_num_not_integer},
                {rec_max_size, 1024 * 1024 * 1024, fun is_integer/1, rec_max_size_not_integer}
        ],
        OptionsVerified = lists:foldl(fun(OptionName, Acc) -> [get_option(OptionName, Options)|Acc] end, [], OptionProps),
        case proplists:get_value(error, OptionsVerified) of
                undefined ->
                        % ok, no error found in options
                        ProcNum = proplists:get_value(proc_num    , OptionsVerified),
                        MaxSize = round(proplists:get_value(rec_max_size, OptionsVerified) / ProcNum),
    			supervisor:start_link({local, ?MODULE}, ?MODULE, [ProcNum, MaxSize]);
                Reason ->
                        % error found in options
                        {error, Reason}
        end.

%% @spec () -> ok |
%%             not_started
%% @doc stop process.
%% @end
stop() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) == true ->
            exit(Pid, shutdown),
            ok;
        _ -> not_started
    end.

get_server_ids() ->
	Props  = supervisor:count_children(?MODULE),
	Active = proplists:get_value(active, Props),
	get_server_ids(Active).

get_server_ids(ProcNum) ->
	lists:map(fun(Index) ->
                          get_server_id(Index)
              end, lists:seq(0, ProcNum-1)).

get_server_id(Index) when is_integer(Index) andalso Index >= 0 ->
	case erlang:get(Index) of
		undefined ->
			NewId = list_to_atom(?SERVER_NAME_PREFIX ++ string:right(integer_to_list(Index), 2, $0)),
			erlang:put(Index, NewId),
			NewId;
		Id ->
			Id
	end;

get_server_id(Key) when is_binary(Key) ->
	Props  = supervisor:count_children(?MODULE),
	Active = proplists:get_value(active, Props),
	Index = erlang:phash2(Key, Active),
	get_server_id(Index);

get_server_id(Key) when is_list(Key) ->
	Bin = list_to_binary(Key),
	get_server_id(Bin);

get_server_id(Key) when is_atom(Key) ->
	Bin = atom_to_binary(Key, latin1),
	get_server_id(Bin).
%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @spec (Params) -> ok
%% @doc stop process.
%% @end
%% @private
init([ProcNum|Args]) ->
	RegisteredProcs = lists:map(
		fun(Id) ->
			{Id, {ecache_server, start_link, [Id, Args]},
			permanent, brutal_kill, worker, [ecache_server]}
		end, get_server_ids(ProcNum)),
	io:format("proc:~p~n", [ProcNum]),
	{ok, {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME}, RegisteredProcs}}.

% ============================ INTERNAL FUNCTIONS ==============================
% Description: Validate and get misultin options.
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
