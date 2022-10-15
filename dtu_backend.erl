-module(dtu_backend).

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).

handle(Req, _Args) ->
  handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"hello">>, <<"world">>], _Req) ->
  {ok, [], <<"Hello World!">>};
  handle('POST', [<<"task">>], Req) ->
    Name = elli_request:post_arg(<<"name">>, Req,
                                   <<"undefined">>),
      io:format("Name: ~s~n", [Name]), {201, [], <<"Created">>};
  handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
  ok.

