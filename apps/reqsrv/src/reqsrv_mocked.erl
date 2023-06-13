-module(reqsrv_mocked).

-export([do/0]).

do() ->
  meck:expect(hackney, request, ['_', '_', '_', '_', '_'], meck:val({ok, 200, [], client})),
  meck:expect(hackney,
    body,
    fun(client) ->
      Response =
        [{<<"Balance1">>, rand:uniform(10)},
          {<<"Balance2">>, rand:uniform(10)}],
      io:format("MOCKED REST request to endpoint, 3s delay~n",[]),
      timer:sleep(3000),
      {ok, jsx:encode(Response)}
    end).
