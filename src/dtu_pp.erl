-module(dtu_pp).

-export([abovel/2, besidel/2, pp_unk_if_not_empty/2, pp_unk/3, atext/2, ntext/2, nestc/2,
         new_ctx/0, quote_string/1, quote_string/2, quote_string_raw/1, quote_string_raw/2,
         join/4]).

-record(ctx, {sub_indent = 2 :: non_neg_integer()}).

-import(prettypr, [beside/2, empty/0, text/1, par/2, nest/2, above/2]).

new_ctx() ->
    #ctx{}.

ast_as_comment(Label, Ast) ->
    text(io_lib:format("/* UNK ~s: ~p */", [Label, Ast])).

nestc(Layout, Ctx) ->
    nest(Ctx#ctx.sub_indent, Layout).

atext(Atom, _Ctx) ->
    text(atom_to_list(Atom)).

ntext(V, _Ctx) ->
    text(io_lib:format("~p", [V])).

pp_unk_if_not_empty([], _Ctx) ->
    empty();
pp_unk_if_not_empty(Ast, Ctx) ->
    pp_unk("not empty", Ast, Ctx).

pp_unk(Label, Ast, _Ctx) ->
    ast_as_comment(Label, Ast).

abovel([], _Ctx) ->
    empty();
abovel([H], _Ctx) ->
    H;
% maybe skip empty() here?
abovel([H | T], Ctx) ->
    above(H, abovel(T, Ctx)).

besidel([], _Ctx) ->
    empty();
besidel([H], _Ctx) ->
    H;
besidel([H | T], Ctx) ->
    beside(H, besidel(T, Ctx)).

quote_string(V) ->
    quote_string(V, $").

quote_string(V, QuoteChar) ->
    text(quote_string_raw(V, QuoteChar)).

quote_string_raw(V) ->
    quote_string_raw(V, $").

quote_string_raw(V, QuoteChar) ->
    io_lib:write_string(V, QuoteChar).

join(Items, Ctx, PPFun, Sep) ->
    join(Items, Ctx, PPFun, Sep, []).

join([], _Ctx, _PPFun, _Sep, []) ->
    empty();
join([Item], Ctx, PPFun, _Sep, []) ->
    PPFun(Item, Ctx);
join([Item], Ctx, PPFun, _Sep, Accum) ->
    par(lists:reverse([PPFun(Item, Ctx) | Accum]), 2);
join([H | T = [_ | _]], Ctx, PPFun, Sep, Accum) ->
    join(T, Ctx, PPFun, Sep, [beside(PPFun(H, Ctx), Sep) | Accum]);
join([H | T], Ctx, PPFun, Sep, Accum) ->
    join([T], Ctx, PPFun, Sep, [beside(PPFun(H, Ctx), Sep) | Accum]).
