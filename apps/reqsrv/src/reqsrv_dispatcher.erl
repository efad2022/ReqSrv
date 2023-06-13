-module(reqsrv_dispatcher).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).
-export([notify_idle_worker/1]).
-export([process_request/2]).
-export([info/0]).


-spec(start_link() -> {ok, pid()}).
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec(notify_idle_worker(pid()) -> ok).
notify_idle_worker(WorkerPid) ->
  gen_server:cast(?MODULE, {notify_idle_worker, WorkerPid}).

-spec(process_request(pid(), binary()) -> ok).
process_request(ClientPid, MSISDN) ->
  gen_server:cast(?MODULE, {process_request, ClientPid, MSISDN}).

%% NOTE: queued_requests does not count MSISDN locked requests
-spec(info() -> #{queued_requests => integer(), idle_workers => integer()}).
info() ->
  gen_server:call(?MODULE, info).


%% gen_server callbacks

init(_Args) ->
  {ok, {queue:new(), _IdleWorkerPids = []}}.

handle_call(info, _From, State = {Queue, IdleWorkerPids}) ->
  Info = #{queued_requests => queue:len(Queue), idle_workers => length(IdleWorkerPids)},
  {reply, Info, State}.

%% get request from the processing queue
%% if queue is empty set the worker pid as idle
handle_cast({notify_idle_worker, IdleWorkerPid}, {Queue, IdleWorkerPids}) ->
  case queue:out(Queue) of
    {{value, Request}, NewQueue} ->
      reqsrv_worker:process_request(IdleWorkerPid, Request),
      {noreply, {NewQueue, IdleWorkerPids}};
    {empty, Queue} ->
      case lists:member(IdleWorkerPid, IdleWorkerPids) of
        false ->
          %% monitor the IdleWorkerPid
          erlang:monitor(process, IdleWorkerPid),
          {noreply, {Queue, [IdleWorkerPid | IdleWorkerPids]}};
        true ->
          %% wait
          {noreply, {Queue, IdleWorkerPids}}
      end
  end;
%% no idle worker then enqueue request
handle_cast(Request = {process_request, _ClientPid, _MSISDN}, {Queue, []}) ->
  NewQueue = queue:in(Request, Queue),
  {noreply, {NewQueue, []}};
%% send request to idle worker pid
handle_cast(Request = {process_request, _ClientPid, _MSISDN}, {Queue, [IdleWorkerPid | IdleWorkerPids]}) ->
  reqsrv_worker:process_request(IdleWorkerPid, Request),
  {noreply, {Queue, IdleWorkerPids}}.

%% remove the idle worker when DOWN
handle_info({'DOWN', _Ref, process, Pid, _Reason}, {Queue, IdleWorkerPids}) ->
  {noreply, {Queue, lists:delete(Pid, IdleWorkerPids)}}.

%% internal functions
