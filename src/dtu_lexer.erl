-file("/nix/store/3z5jqm50yirw2x2qlffx8k8pr9ivp4kp-erlang-24.2/lib/erlang/lib/parsetools-2.3.2/include/leexinc.hrl", 0).
%% The source of this file is part of leex distribution, as such it
%% has the same Copyright as the other files in the leex
%% distribution. The Copyright is defined in the accompanying file
%% COPYRIGHT. However, the resultant scanner generated by leex is the
%% property of the creator of the scanner and is not covered by that
%% Copyright.

-module(dtu_lexer).

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% User code. This is placed here to allow extra attributes.
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 61).

make_token(Name, Line, Chars) when is_list(Chars) ->
    {token, {Name, Line, list_to_atom(Chars)}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_token(Name, Line, Chars, Fun) ->
    {token, {Name, Line, Fun(Chars)}}.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2), Line),
    {token, {Type, Line, String}}.

unescape_string(String, Line) -> unescape_string(String, Line, []).

unescape_string([], _Line, Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Line, Output) ->
  Char = map_escaped_char(Escaped, Line),
  unescape_string(Rest, Line, [Char|Output]);
unescape_string([Char|Rest], Line, Output) ->
  unescape_string(Rest, Line, [Char|Output]).

map_escaped_char(Escaped, Line) ->
  case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $\( -> $(;
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {Line, fn_lexer, ["unrecognized escape sequence: ", [$\\, Escaped]]}})
  end.

-file("/nix/store/3z5jqm50yirw2x2qlffx8k8pr9ivp4kp-erlang-24.2/lib/erlang/lib/parsetools-2.3.2/include/leexinc.hrl", 14).

format_error({illegal,S}) -> ["illegal characters ",io_lib:write_string(S)];
format_error({user,S}) -> S.

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

%% string(InChars, Line, TokenChars, Tokens) ->
%% {ok,Tokens,Line} | {error,ErrorInfo,Line}.
%% Note the line number going into yystate, L0, is line of token
%% start while line number returned is line of token end. We want line
%% of token start.

string([], L, [], Ts) ->                     % No partial tokens!
    {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
    case yystate(yystate(), Ics0, L0, 0, reject, 0) of
        {A,Alen,Ics1,L1} ->                  % Accepting end state
            string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
        {A,Alen,Ics1,L1,_S1} ->              % Accepting transistion state
            string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
        {reject,_Alen,Tlen,_Ics1,L1,_S1} ->  % After a non-accepting state
            {error,{L0,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
        {A,Alen,Tlen,_Ics1,L1,_S1} ->
            Tcs1 = yysuf(Tcs, Alen),
            L2 = adjust_line(Tlen, Alen, Tcs1, L1),
            string_cont(Tcs1, L2, yyaction(A, Alen, Tcs, L0), Ts)
    end.

%% string_cont(RestChars, Line, Token, Tokens)
%% Test for and remove the end token wrapper. Push back characters
%% are prepended to RestChars.

-dialyzer({nowarn_function, string_cont/4}).

string_cont(Rest, Line, {token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
    string(Rest, Line, Rest, Ts);
string_cont(Rest, Line, {skip_token,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, Ts);
string_cont(_Rest, Line, {error,S}, _Ts) ->
    {error,{Line,?MODULE,{user,S}},Line}.

%% token(Continuation, Chars) ->
%% token(Continuation, Chars, Line) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept. The continuation is:
%% {token,State,CurrLine,TokenChars,TokenLen,TokenLine,AccAction,AccLen}

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
    token(yystate(), Chars, Line, Chars, 0, Line, reject, 0);
token({token,State,Line,Tcs,Tlen,Tline,Action,Alen}, Chars, _) ->
    token(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Action, Alen).

%% token(State, InChars, Line, TokenChars, TokenLen, TokenLine,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% The argument order is chosen to be more efficient.

token(S0, Ics0, L0, Tcs, Tlen0, Tline, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        %% Accepting end state, we have a token.
        {A1,Alen1,Ics1,L1} ->
            token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
        %% Accepting transition state, can take more chars.
        {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
            {more,{token,S1,L1,Tcs,Alen1,Tline,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
            token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
        %% After a non-accepting state, maybe reach accept state later.
        {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
            {more,{token,S1,L1,Tcs,Tlen1,Tline,A1,Alen1}};
        {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
            %% Check for partial token which is error.
            Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
                                          %% Skip eof tail in Tcs.
                                          {illegal,yypre(Tcs, Tlen1)}},L1};
                     true -> {eof,L1}
                  end,
            {done,Ret,eof};
        {reject,_Alen1,Tlen1,Ics1,L1,_S1} ->    % No token match
            Error = {Tline,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
            {done,{error,Error,L1},Ics1};
        {A1,Alen1,Tlen1,_Ics1,L1,_S1} ->       % Use last accept match
            Tcs1 = yysuf(Tcs, Alen1),
            L2 = adjust_line(Tlen1, Alen1, Tcs1, L1),
            token_cont(Tcs1, L2, yyaction(A1, Alen1, Tcs, Tline))
    end.

%% token_cont(RestChars, Line, Token)
%% If we have a token or error then return done, else if we have a
%% skip_token then continue.

-dialyzer({nowarn_function, token_cont/3}).

token_cont(Rest, Line, {token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {token,T,Push}) ->
    NewRest = Push ++ Rest,
    {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, {end_token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T,Push}) ->
    NewRest = Push ++ Rest,
    {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, skip_token) ->
    token(yystate(), Rest, Line, Rest, 0, Line, reject, 0);
token_cont(Rest, Line, {skip_token,Push}) ->
    NewRest = Push ++ Rest,
    token(yystate(), NewRest, Line, NewRest, 0, Line, reject, 0);
token_cont(Rest, Line, {error,S}) ->
    {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

%% tokens(Continuation, Chars, Line) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept. The continuation is:
%% {tokens,State,CurrLine,TokenChars,TokenLen,TokenLine,Tokens,AccAction,AccLen}
%% {skip_tokens,State,CurrLine,TokenChars,TokenLen,TokenLine,Error,AccAction,AccLen}

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
    tokens(yystate(), Chars, Line, Chars, 0, Line, [], reject, 0);
tokens({tokens,State,Line,Tcs,Tlen,Tline,Ts,Action,Alen}, Chars, _) ->
    tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Ts, Action, Alen);
tokens({skip_tokens,State,Line,Tcs,Tlen,Tline,Error,Action,Alen}, Chars, _) ->
    skip_tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Error, Action, Alen).

%% tokens(State, InChars, Line, TokenChars, TokenLen, TokenLine, Tokens,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.

tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Ts, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        %% Accepting end state, we have a token.
        {A1,Alen1,Ics1,L1} ->
            tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
        %% Accepting transition state, can take more chars.
        {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
            {more,{tokens,S1,L1,Tcs,Alen1,Tline,Ts,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
            tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
        %% After a non-accepting state, maybe reach accept state later.
        {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
            {more,{tokens,S1,L1,Tcs,Tlen1,Tline,Ts,A1,Alen1}};
        {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
            %% Check for partial token which is error, no need to skip here.
            Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
                                          %% Skip eof tail in Tcs.
                                          {illegal,yypre(Tcs, Tlen1)}},L1};
                     Ts == [] -> {eof,L1};
                     true -> {ok,yyrev(Ts),L1}
                  end,
            {done,Ret,eof};
        {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
            %% Skip rest of tokens.
            Error = {L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
            skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
        {A1,Alen1,Tlen1,_Ics1,L1,_S1} ->
            Token = yyaction(A1, Alen1, Tcs, Tline),
            Tcs1 = yysuf(Tcs, Alen1),
            L2 = adjust_line(Tlen1, Alen1, Tcs1, L1),
            tokens_cont(Tcs1, L2, Token, Ts)
    end.

%% tokens_cont(RestChars, Line, Token, Tokens)
%% If we have an end_token or error then return done, else if we have
%% a token then save it and continue, else if we have a skip_token
%% just continue.

-dialyzer({nowarn_function, tokens_cont/4}).

tokens_cont(Rest, Line, {token,T}, Ts) ->
    tokens(yystate(), Rest, Line, Rest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    tokens(yystate(), NewRest, Line, NewRest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
    {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, {end_token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    {done,{ok,yyrev(Ts, [T]),Line},NewRest};
tokens_cont(Rest, Line, skip_token, Ts) ->
    tokens(yystate(), Rest, Line, Rest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {skip_token,Push}, Ts) ->
    NewRest = Push ++ Rest,
    tokens(yystate(), NewRest, Line, NewRest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, _Ts) ->
    skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

%%skip_tokens(InChars, Line, Error) -> {done,{error,Error,Line},Ics}.
%% Skip tokens until an end token, junk everything and return the error.

skip_tokens(Ics, Line, Error) ->
    skip_tokens(yystate(), Ics, Line, Ics, 0, Line, Error, reject, 0).

%% skip_tokens(State, InChars, Line, TokenChars, TokenLen, TokenLine, Tokens,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.

skip_tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Error, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        {A1,Alen1,Ics1,L1} ->                  % Accepting end state
            skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
        {A1,Alen1,[],L1,S1} ->                 % After an accepting state
            {more,{skip_tokens,S1,L1,Tcs,Alen1,Tline,Error,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->
            skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
        {A1,Alen1,Tlen1,[],L1,S1} ->           % After a non-accepting state
            {more,{skip_tokens,S1,L1,Tcs,Tlen1,Tline,Error,A1,Alen1}};
        {reject,_Alen1,_Tlen1,eof,L1,_S1} ->
            {done,{error,Error,L1},eof};
        {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
            skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
        {A1,Alen1,Tlen1,_Ics1,L1,_S1} ->
            Token = yyaction(A1, Alen1, Tcs, Tline),
            Tcs1 = yysuf(Tcs, Alen1),
            L2 = adjust_line(Tlen1, Alen1, Tcs1, L1),
            skip_cont(Tcs1, L2, Token, Error)
    end.

%% skip_cont(RestChars, Line, Token, Error)
%% Skip tokens until we have an end_token or error then return done
%% with the original rror.

-dialyzer({nowarn_function, skip_cont/4}).

skip_cont(Rest, Line, {token,_T}, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {token,_T,Push}, Error) ->
    NewRest = Push ++ Rest,
    skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {end_token,_T}, Error) ->
    {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {end_token,_T,Push}, Error) ->
    NewRest = Push ++ Rest,
    {done,{error,Error,Line},NewRest};
skip_cont(Rest, Line, skip_token, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {skip_token,Push}, Error) ->
    NewRest = Push ++ Rest,
    skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {error,_S}, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0).

-compile({nowarn_unused_function, [yyrev/1, yyrev/2, yypre/2, yysuf/2]}).

yyrev(List) -> lists:reverse(List).
yyrev(List, Tail) -> lists:reverse(List, Tail).
yypre(List, N) -> lists:sublist(List, N).
yysuf(List, N) -> lists:nthtail(N, List).

%% adjust_line(TokenLength, AcceptLength, Chars, Line) -> NewLine
%% Make sure that newlines in Chars are not counted twice.
%% Line has been updated with respect to newlines in the prefix of
%% Chars consisting of (TokenLength - AcceptLength) characters.

-compile({nowarn_unused_function, adjust_line/4}).

adjust_line(N, N, _Cs, L) -> L;
adjust_line(T, A, [$\n|Cs], L) ->
    adjust_line(T-1, A, Cs, L-1);
adjust_line(T, A, [_|Cs], L) ->
    adjust_line(T-1, A, Cs, L).

%% yystate() -> InitialState.
%% yystate(State, InChars, Line, CurrTokLen, AcceptAction, AcceptLen) ->
%% {Action, AcceptLen, RestChars, Line} |
%% {Action, AcceptLen, RestChars, Line, State} |
%% {reject, AcceptLen, CurrTokLen, RestChars, Line, State} |
%% {Action, AcceptLen, CurrTokLen, RestChars, Line, State}.
%% Generated state transition functions. The non-accepting end state
%% return signal either an unrecognised character or end of current
%% input.

-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.erl", 348).
yystate() -> 26.

yystate(29, Ics, Line, Tlen, _, _) ->
    {14,Tlen,Ics,Line};
yystate(28, [32|Ics], Line, Tlen, _, _) ->
    yystate(16, Ics, Line, Tlen+1, 18, Tlen);
yystate(28, [13|Ics], Line, Tlen, _, _) ->
    yystate(20, Ics, Line, Tlen+1, 18, Tlen);
yystate(28, [10|Ics], Line, Tlen, _, _) ->
    yystate(24, Ics, Line+1, Tlen+1, 18, Tlen);
yystate(28, [9|Ics], Line, Tlen, _, _) ->
    yystate(28, Ics, Line, Tlen+1, 18, Tlen);
yystate(28, Ics, Line, Tlen, _, _) ->
    {18,Tlen,Ics,Line,28};
yystate(27, Ics, Line, Tlen, _, _) ->
    {12,Tlen,Ics,Line};
yystate(26, [125|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [123|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [95|Ics], Line, Tlen, Action, Alen) ->
    yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [93|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [91|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [58|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [47|Ics], Line, Tlen, Action, Alen) ->
    yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [46|Ics], Line, Tlen, Action, Alen) ->
    yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [44|Ics], Line, Tlen, Action, Alen) ->
    yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [40|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [35|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [32|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [13|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line+1, Tlen+1, Action, Alen);
yystate(26, [9|Ics], Line, Tlen, Action, Alen) ->
    yystate(28, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
    yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
    yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(26, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,26};
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(25, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,25};
yystate(24, Ics, Line, Tlen, _, _) ->
    {16,Tlen,Ics,Line};
yystate(23, Ics, Line, Tlen, _, _) ->
    {11,Tlen,Ics,Line};
yystate(22, Ics, Line, Tlen, _, _) ->
    {10,Tlen,Ics,Line};
yystate(21, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(21, Ics, Line, Tlen+1, 1, Tlen);
yystate(21, Ics, Line, Tlen, _, _) ->
    {1,Tlen,Ics,Line,21};
yystate(20, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line+1, Tlen+1, Action, Alen);
yystate(20, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,20};
yystate(19, Ics, Line, Tlen, _, _) ->
    {6,Tlen,Ics,Line};
yystate(18, Ics, Line, Tlen, _, _) ->
    {9,Tlen,Ics,Line};
yystate(17, [45|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [43|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(17, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,17};
yystate(16, [32|Ics], Line, Tlen, Action, Alen) ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [13|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line+1, Tlen+1, Action, Alen);
yystate(16, [9|Ics], Line, Tlen, Action, Alen) ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,16};
yystate(15, Ics, Line, Tlen, _, _) ->
    {5,Tlen,Ics,Line};
yystate(14, [95|Ics], Line, Tlen, _, _) ->
    yystate(14, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(14, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [C|Ics], Line, Tlen, _, _) when C >= 64, C =< 90 ->
    yystate(14, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(14, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, Ics, Line, Tlen, _, _) ->
    {4,Tlen,Ics,Line,14};
yystate(13, [101|Ics], Line, Tlen, _, _) ->
    yystate(17, Ics, Line, Tlen+1, 1, Tlen);
yystate(13, [69|Ics], Line, Tlen, _, _) ->
    yystate(17, Ics, Line, Tlen+1, 1, Tlen);
yystate(13, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(13, Ics, Line, Tlen+1, 1, Tlen);
yystate(13, Ics, Line, Tlen, _, _) ->
    {1,Tlen,Ics,Line,13};
yystate(12, [32|Ics], Line, Tlen, _, _) ->
    yystate(12, Ics, Line, Tlen+1, 17, Tlen);
yystate(12, [13|Ics], Line, Tlen, _, _) ->
    yystate(20, Ics, Line, Tlen+1, 17, Tlen);
yystate(12, [10|Ics], Line, Tlen, _, _) ->
    yystate(24, Ics, Line+1, Tlen+1, 17, Tlen);
yystate(12, [9|Ics], Line, Tlen, _, _) ->
    yystate(16, Ics, Line, Tlen+1, 17, Tlen);
yystate(12, Ics, Line, Tlen, _, _) ->
    {17,Tlen,Ics,Line,12};
yystate(11, Ics, Line, Tlen, _, _) ->
    {13,Tlen,Ics,Line};
yystate(10, Ics, Line, Tlen, _, _) ->
    {8,Tlen,Ics,Line};
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(9, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,9};
yystate(8, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line+1, Tlen+1, Action, Alen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 33 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= 35, C =< 91 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= 93 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(8, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,8};
yystate(7, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line+1, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 33 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= 35, C =< 91 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= 93 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(7, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,7};
yystate(6, Ics, Line, Tlen, _, _) ->
    {7,Tlen,Ics,Line};
yystate(5, [46|Ics], Line, Tlen, _, _) ->
    yystate(9, Ics, Line, Tlen+1, 0, Tlen);
yystate(5, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(5, Ics, Line, Tlen+1, 0, Tlen);
yystate(5, Ics, Line, Tlen, _, _) ->
    {0,Tlen,Ics,Line,5};
yystate(4, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line};
yystate(3, [94|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(3, [93|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(3, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(3, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(3, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line+1, Tlen+1, Action, Alen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 33 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= 35, C =< 91 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= 95 ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(3, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,3};
yystate(2, [95|Ics], Line, Tlen, _, _) ->
    yystate(2, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(2, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
    yystate(2, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(2, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, Ics, Line, Tlen, _, _) ->
    {3,Tlen,Ics,Line,2};
yystate(1, Ics, Line, Tlen, _, _) ->
    {15,Tlen,Ics,Line};
yystate(0, [92|Ics], Line, Tlen, _, _) ->
    yystate(3, Ics, Line, Tlen+1, 2, Tlen);
yystate(0, [34|Ics], Line, Tlen, _, _) ->
    yystate(4, Ics, Line, Tlen+1, 2, Tlen);
yystate(0, [10|Ics], Line, Tlen, _, _) ->
    yystate(8, Ics, Line+1, Tlen+1, 2, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 0, C =< 9 ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 11, C =< 33 ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 35, C =< 91 ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 93 ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(0, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.

%% yyaction(Action, TokenLength, TokenChars, TokenLine) ->
%% {token,Token} | {end_token, Token} | skip_token | {error,String}.
%% Generated action function.

yyaction(0, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_0(TokenChars, TokenLine);
yyaction(1, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_1(TokenChars, TokenLine);
yyaction(2, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_2(TokenChars, TokenLen, TokenLine);
yyaction(3, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_3(TokenChars, TokenLine);
yyaction(4, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_4(TokenChars, TokenLine);
yyaction(5, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_5(TokenChars, TokenLine);
yyaction(6, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_6(TokenChars, TokenLine);
yyaction(7, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_7(TokenChars, TokenLine);
yyaction(8, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_8(TokenChars, TokenLine);
yyaction(9, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_9(TokenChars, TokenLine);
yyaction(10, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_10(TokenChars, TokenLine);
yyaction(11, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_11(TokenChars, TokenLine);
yyaction(12, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_12(TokenChars, TokenLine);
yyaction(13, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_13(TokenChars, TokenLine);
yyaction(14, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_14(TokenChars, TokenLine);
yyaction(15, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_15(TokenChars, TokenLine);
yyaction(16, _, _, _) ->
    yyaction_16();
yyaction(17, _, _, _) ->
    yyaction_17();
yyaction(18, _, _, _) ->
    yyaction_18();
yyaction(_, _, _, _) -> error.

-compile({inline,yyaction_0/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 32).
yyaction_0(TokenChars, TokenLine) ->
     make_token (integer, TokenLine, TokenChars, fun erlang : list_to_integer / 1) .

-compile({inline,yyaction_1/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 33).
yyaction_1(TokenChars, TokenLine) ->
     make_token (float, TokenLine, TokenChars, fun erlang : list_to_float / 1) .

-compile({inline,yyaction_2/3}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 34).
yyaction_2(TokenChars, TokenLen, TokenLine) ->
     build_string (string, TokenChars, TokenLine, TokenLen) .

-compile({inline,yyaction_3/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 36).
yyaction_3(TokenChars, TokenLine) ->
     make_token (uname, TokenLine, TokenChars) .

-compile({inline,yyaction_4/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 37).
yyaction_4(TokenChars, TokenLine) ->
     make_token (lname, TokenLine, TokenChars) .

-compile({inline,yyaction_5/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 39).
yyaction_5(TokenChars, TokenLine) ->
     make_token (open, TokenLine, TokenChars) .

-compile({inline,yyaction_6/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 40).
yyaction_6(TokenChars, TokenLine) ->
     make_token (close, TokenLine, TokenChars) .

-compile({inline,yyaction_7/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 42).
yyaction_7(TokenChars, TokenLine) ->
     make_token (open_list, TokenLine, TokenChars) .

-compile({inline,yyaction_8/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 43).
yyaction_8(TokenChars, TokenLine) ->
     make_token (close_list, TokenLine, TokenChars) .

-compile({inline,yyaction_9/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 45).
yyaction_9(TokenChars, TokenLine) ->
     make_token (open_map, TokenLine, TokenChars) .

-compile({inline,yyaction_10/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 46).
yyaction_10(TokenChars, TokenLine) ->
     make_token (close_map, TokenLine, TokenChars) .

-compile({inline,yyaction_11/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 48).
yyaction_11(TokenChars, TokenLine) ->
     make_token (sep, TokenLine, TokenChars) .

-compile({inline,yyaction_12/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 49).
yyaction_12(TokenChars, TokenLine) ->
     make_token (dot, TokenLine, TokenChars) .

-compile({inline,yyaction_13/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 50).
yyaction_13(TokenChars, TokenLine) ->
     make_token (hash, TokenLine, TokenChars) .

-compile({inline,yyaction_14/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 51).
yyaction_14(TokenChars, TokenLine) ->
     make_token (slash, TokenLine, TokenChars) .

-compile({inline,yyaction_15/2}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 52).
yyaction_15(TokenChars, TokenLine) ->
     make_token (colon, TokenLine, TokenChars) .

-compile({inline,yyaction_16/0}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 55).
yyaction_16() ->
     skip_token .

-compile({inline,yyaction_17/0}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 56).
yyaction_17() ->
     skip_token .

-compile({inline,yyaction_18/0}).
-file("/home/mariano/src/spawnfest/dtu/src/dtu_lexer.xrl", 57).
yyaction_18() ->
     skip_token .

-file("/nix/store/3z5jqm50yirw2x2qlffx8k8pr9ivp4kp-erlang-24.2/lib/erlang/lib/parsetools-2.3.2/include/leexinc.hrl", 313).
