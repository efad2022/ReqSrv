-module(reqsrv_workers_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(NWorkers) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [NWorkers]).

init([NWorkers]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 1,
    period => 5},
  ChildSpecs = lists:map(fun(Id) ->
    {Id, {reqsrv_worker, start_link, []}, permanent, 5000, worker, [?MODULE]} end, lists:seq(1, NWorkers)),
  {ok, {SupFlags, ChildSpecs}}.

