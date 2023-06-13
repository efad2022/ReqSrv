-module(reqsrv_app).

-ifdef('TEST').
-define(MOCK(), reqsrv_mocked:do()).
-else.
-define(MOCK(), false).
-endif.

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

  %% NOTE: Mock is only available under the test profile
  ?MOCK(),

  %% connect to nodes
  net_adm:world(),

  %% Config
  SrvPort = application:get_env(reqsrv, port, 9100),
  NWorkers = application:get_env(reqsrv, workers_num, 5),

  %% Cowboy routes
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/:msisdn", msisdn_get_handler, []}
    ]}
  ]),
  {ok, _} =
    cowboy:start_clear(rest_server_listener,
      [{port, SrvPort}],
      #{env => #{dispatch => Dispatch}
      }),
  reqsrv_sup:start_link(NWorkers).

stop(_State) ->
  ok.


