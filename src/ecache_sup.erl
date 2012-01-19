-module(ecache_sup).
-vsn('0.0.1').
-behaviour(supervisor).

%% External API
-export([start_link/1, stop/0, get_server_id/1, get_server_ids/0, server_ids_foldl/2]).

%% Callbacks
-export([init/1]).

-define(SHUTDOWN_WAITING_TIME,          2000).
-define(MAX_RESTART,                       5).
-define(MAX_TIME,                         60).
-define(SERVER_NUM_PROCS,                 16).
-define(SERVER_NAME_PREFIX, "ecache_server_").

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @spec (Params) -> ok
%% @doc start link.
%% @end
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

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

server_ids_foldl(Fun, Acc) when is_function(Fun) ->
	lists:foldl(Fun, Acc, get_server_ids()).

get_server_ids() ->
    lists:map(fun(Index) ->
                          get_server_id(Index)
              end, lists:seq(0, ?SERVER_NUM_PROCS-1)).

get_server_id(Index) when is_integer(Index) andalso Index >= 0 andalso Index < ?SERVER_NUM_PROCS ->
	list_to_atom(?SERVER_NAME_PREFIX ++ string:right(integer_to_list(Index), 2, $0));

get_server_id(Key) when is_binary(Key) ->
	Index = erlang:phash2(Key, ?SERVER_NUM_PROCS),
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
init(Args) ->
	RegisteredProcs = lists:map(
		fun(Id) ->
			{Id, {ecache_server, start_link, [Id, Args]},
			permanent, brutal_kill, worker, [ecache_server]}
		end, get_server_ids()),
	{ok, {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME}, RegisteredProcs}}.
