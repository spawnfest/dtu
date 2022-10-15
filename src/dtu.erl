-module(dtu).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Code]) ->
    io:format("Result: ~p~n    ~p~n", [Code, parse_string(Code)]),
    erlang:halt(0).

parse_string(Str) ->
    case lex_string(Str) of
        {ok, Tokens} ->
            dtu_parser:parse(Tokens);
        Other ->
            Other
    end.

lex_string(Str) ->
    case dtu_lexer:string(Str) of
        {ok, Tokens, _Endline} ->
            {ok, Tokens};
        {eof, Endline} ->
            {error, {Endline, dtu_lexer, {eof, Endline}}};
        {error, Error} ->
            {error, Error};
        {error, Error, _} ->
            {error, Error}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
