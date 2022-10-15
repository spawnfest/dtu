Definitions.

Number = [0-9]
Float  = [0-9]+\.[0-9]+([eE][-+]?[0-9]+)?

String = "(\\\^.|\\.|[^\"])*"

UName  = [A-Z\_][a-zA-Z0-9\_\-]*
LName  = [a-z][a-zA-Z0-9\_\-]*

Symbol = (<|=|>|!|%|&|\?|\*|-|\+)+

Open        = \(
Close       = \)

OpenList    = \[
CloseList   = \]

OpenMap     = \{
CloseMap    = \}

Sep         = ,
Dot         = \.
Hash        = #
Slash       = /
Colon       = :

String      = "(\\\^.|\\.|[^\"])*"

Endls       = (\s|\t)*(\r?\n)
Whites      = \s+
Tabs        = \t+

Rules.

{Number}+               : make_token(integer, TokenLine, TokenChars, fun erlang:list_to_integer/1).
{Float}                 : make_token(float, TokenLine, TokenChars, fun erlang:list_to_float/1).
{String}                : build_string(string, TokenChars, TokenLine, TokenLen).

{UName}                 : make_token(uname, TokenLine, TokenChars).
{LName}                 : make_token(lname, TokenLine, TokenChars).

{Open}                  : make_token(open, TokenLine, TokenChars).
{Close}                 : make_token(close, TokenLine, TokenChars).

{OpenList}              : make_token(open_list, TokenLine, TokenChars).
{CloseList}             : make_token(close_list, TokenLine, TokenChars).

{OpenMap}               : make_token(open_map, TokenLine, TokenChars).
{CloseMap}              : make_token(close_map, TokenLine, TokenChars).

{Sep}                   : make_token(sep, TokenLine, TokenChars).
{Dot}                   : make_token(dot, TokenLine, TokenChars).
{Hash}                  : make_token(hash, TokenLine, TokenChars).
{Slash}                 : make_token(slash, TokenLine, TokenChars).
{Colon}                 : make_token(colon, TokenLine, TokenChars).
{Symbol}                : make_token(symbol, TokenLine, TokenChars).

% spaces, tabs and new lines
{Endls}                 : skip_token.
{Whites}                : skip_token.
{Tabs}                  : skip_token.

Erlang code.

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
