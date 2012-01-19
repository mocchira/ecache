-module(ecache_app).
-behaviour(application).
-vsn("0.0.1").

-export([start/2, stop/1, profile_output/0]). %% API.

-type application_start_type() :: normal
	| {takeover, node()} | {failover, node()}.

%% API.

-spec start(application_start_type(), any()) -> {ok, pid()}.
start(_Type, Args) ->
	consider_profiling(),
	ecache_sup:start_link(Args).

-spec stop(any()) -> ok.
stop(_State) ->
	ecache_sup:stop(),
	ok.

-spec profile_output() -> ok.
profile_output() ->
	eprof:stop_profiling(),
	eprof:log("procs.profile"),
	eprof:analyze(procs),
	eprof:log("total.profile"),
	eprof:analyze(total).

%% Internal.

-spec consider_profiling() -> profiling | not_profiling.
consider_profiling() ->
	case application:get_env(profile) of
		{ok, true} ->
			{ok, _Pid} = eprof:start(),
			eprof:start_profiling([self()]);
		_ ->
			not_profiling
	end.

