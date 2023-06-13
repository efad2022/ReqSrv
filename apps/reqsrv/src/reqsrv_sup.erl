-module(reqsrv_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(NWorkers) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [NWorkers]).

init([NWorkers]) ->
  SupFlags = #{strategy => one_for_all,
    intensity => 1,
    period => 5},
  ChildsSpec = [{dispatcher, {reqsrv_dispatcher, start_link, []}, permanent, 5000, worker, [?MODULE]},
    {workers_sup, {reqsrv_workers_sup, start_link, [NWorkers]}, permanent, 5000, supervisor, [?MODULE]}],
  {ok, {SupFlags, ChildsSpec}}.


%% internal functions
