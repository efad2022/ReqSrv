-module(msisdn_get_handler).
-behavior(cowboy_rest).

%% REST Callbacks
-export([init/2]).
-export([content_types_provided/2]).
-export([msisdn_request/2]).
%% Cowboy loop Callbacks
-export([info/3]).

-define(REJECT_HTTP_CODE, 400).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, Opts) ->
  {[
    {{<<"application">>, <<"json">>, []}, msisdn_request}
  ], Req, Opts}.

msisdn_request(Req, _Opts) ->
  MSISDN = cowboy_req:binding(msisdn, Req),
  case is_minority_network() of
    true ->
      reject(Req, MSISDN);
    false ->
      try_msisdn_locking(MSISDN),
      {{switch_handler, cowboy_loop}, Req, MSISDN}
  end.

%% may be MSISDN lock is released
info({'DOWN', _Ref, process, _OwnerPid, _Reason}, Req, MSISDN) ->
  case is_minority_network() of
    true ->
      reject(Req, MSISDN);
    false ->
      try_msisdn_locking(MSISDN),
      {ok, Req, MSISDN}
  end;

%% reply from worker with response
info({worker_reply, Response}, Req, MSISDN) ->
  #{<<"Balance1">> := Balance1, <<"Balance2">> := Balance2} = Response,
  Sum = Balance1 + Balance2,
  io:format("[DEBUG] MSISDN:~p. Request returning:~p~n", [MSISDN, Sum]),
  Body = jsx:encode(#{<<"Sum">> => Sum}),
  cowboy_req:reply(200, #{}, Body, Req),
  {stop, Req, MSISDN}.


%% internal functions

is_minority_network() ->
  length(erlang:nodes()) < (length(application:get_env(reqsrv, nodes, [])) div 2).

try_msisdn_locking(MSISDN) ->
  %% NOTE: When new nodes are added to the network, they are informed of
  %% the globally registered names that already exist.
  %% The network is also informed of any global names in newly connected nodes.
  %% If any name clashes are discovered, function Resolve is called.
  %% Its purpose is to decide which pid is correct. If the function crashes,
  %% or returns anything other than one of the pids, the name is unregistered.
  %% This function is called once for each name clash.
  %% Default global:register_name resolve is 'random_exit_name/3'
  %% This means that one of the two registered processes
  %% is selected as correct while the other is killed.
  case global:register_name(MSISDN, self()) of
    yes ->
      reqsrv_dispatcher:process_request(self(), MSISDN),
      yes;
    no ->
      OwnerPid = global:whereis_name(MSISDN),
      erlang:monitor(process, OwnerPid),
      no
  end.

reject(Req, MSISDN) ->
  {stop, cowboy_req:reply(?REJECT_HTTP_CODE, Req), MSISDN}.
