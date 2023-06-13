-module(reqsrv_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3]).
-export([process_request/2]).


-spec(start_link() -> {ok, pid()}).
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec(process_request(pid(), {process_request, pid(), binary()}) -> ok).
process_request(WorkerPid, Request={process_request, _ClientPid, _MSISDN}) ->
  gen_server:cast(WorkerPid, Request).


%% gen_server callbacks

init(_Args) ->
  reqsrv_dispatcher:notify_idle_worker(self()),
  {ok, undefined}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

%% process request and reply back to the client with the response
handle_cast({process_request, ClientPid, MSISDN}, State) ->
  %% NOTE: this is to handle premature client terminations
  link(ClientPid),
  {ok, TargetURL} = application:get_env(reqsrv, target_url),
  %% NOTE: improvement, a client pool can be used here
  Url = restc:construct_url(TargetURL, binary_to_list(MSISDN), []),
  {ok, 200, _, Response} = restc:request(get, Url),
  ClientPid ! {worker_reply, Response},
  %% notify as idle
  reqsrv_dispatcher:notify_idle_worker(self()),
  {noreply, State}.


