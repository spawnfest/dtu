-module(dtu).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Code]) ->
    io:format("Result: ~p~n    ~p~n", [Code, parse_string(Code)]),
    erlang:halt(0);
main(["file", Path]) ->
    io:format("Result:~n~p~n", [parse_file(Path)]),
    erlang:halt(0);
main(["css", Path]) ->
    compile_and_print_path(Path, fun dtu_css:format/1),
    erlang:halt(0).

compile_and_print_path(Path, Fn) ->
    case compile_path(Path, Fn) of
        {ok, R} ->
            io:format("~s~n", [R]);
        Other ->
            io:format("Error: ~p~n", [Other])
    end.

compile_path(Path, Fn) ->
    case parse_file(Path) of
        {ok, Ast} ->
            {ok, Fn(Ast)};
        Other ->
            Other
    end.

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            {ok, unicode:characters_to_list(Content, utf8)};
        Other ->
            Other
    end.

parse_file(Path) ->
    case read_file(Path) of
        {ok, Content} ->
            parse_string(Content);
        Other ->
            Other
    end.

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
