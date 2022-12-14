-module(dtu_css).

-export([format/1, format/2, pp_root/2, pp_rules/2]).

-define(PAPER, 80).
-define(RIBBON, 56).

-import(prettypr, [text/1, sep/1, beside/2]).
-import(dtu_pp,
        [abovel/2,
         besidel/2,
         pp_unk_if_not_empty/2,
         pp_unk/3,
         atext/2,
         ntext/2,
         join/4,
         join_no_sep/4,
         nestc/2,
         quote_string/1]).

format(RootNodes) ->
    format(RootNodes, dtu_pp:new_ctx()).

format(RootNodes, Ctx) ->
    format(RootNodes, Ctx, ?PAPER, ?RIBBON).

format(RootNodes, Ctx, Paper, Ribbon) ->
    prettypr:format(pp_root(RootNodes, Ctx), Paper, Ribbon).

pp_root(Nodes, Ctx) ->
    abovel([pp(Node, Ctx) || Node <- Nodes], Ctx).

pp({node, _Line, {{lqname, _L1, [{lname, _L2, any}]}, Sels, Body}}, Ctx) ->
    abovel([beside(join(Sels, Ctx, fun pp_sel/2, text(", ")), text(" {")),
            nestc(pp_sel_body(Body, Ctx), Ctx),
            text("}")],
           Ctx);
pp({node, _Line, {QName, Head, Body}}, Ctx) ->
    abovel([besidel([pp_sel(QName, Ctx), pp_unk_if_not_empty(Head, Ctx), text(" {")], Ctx),
            nestc(pp_sel_body(Body, Ctx), Ctx),
            text("}")],
           Ctx);
pp(Ast, Ctx) ->
    pp_unk("top", Ast, Ctx).

% {lqname,2,[{uname,2,'All'}]}
pp_sel({lqname, _, [{uname, _, 'All'}]}, _Ctx) ->
    text("*");
pp_sel({lqname, _, [{uname, _, 'Root'}]}, _Ctx) ->
    text(":root");
pp_sel({lqname, _, [{lname, _, Name}]}, Ctx) ->
    atext(Name, Ctx);
pp_sel({expr, _, {Left, Op, Right}}, Ctx) ->
    sep([pp_sel(Left, Ctx), atext(Op, Ctx), pp_sel(Right, Ctx)]);
pp_sel({lqname, _L1, [{lname, _L2, class}, {lname, _L3, Name}]}, Ctx) ->
    beside(text("."), atext(Name, Ctx));
pp_sel({lqname, _L1, [{lname, _L2, id}, {lname, _L3, Name}]}, Ctx) ->
    beside(text("#"), atext(Name, Ctx));
pp_sel(Ast, Ctx) ->
    pp_unk("sel", Ast, Ctx).

pp_sel_body({seq, _Line, Rules}, Ctx) ->
    pp_rules(Rules, Ctx);
pp_sel_body(Ast, Ctx) ->
    pp_unk("sel body", Ast, Ctx).

pp_rules(Rules, Ctx) ->
    abovel([pp_rule(Rule, Ctx) || Rule <- Rules], Ctx).

pp_rule({pair, _L1, {Key, Val}}, Ctx) ->
    besidel([pp_rule_key(Key, Ctx), text(": "), pp_rule_val(Val, Ctx), text(";")], Ctx);
pp_rule({node, _L1, {Key, Vals, BodyUnk}}, Ctx) ->
    besidel([pp_rule_key(Key, Ctx),
             text(": "),
             sep([pp_rule_val(Item, Ctx) || Item <- Vals]),
             pp_unk_if_not_empty_seq(BodyUnk, Ctx),
             text(";")],
            Ctx);
pp_rule(Ast, Ctx) ->
    pp_unk("rule", Ast, Ctx).

pp_rule_key({lqname, _L1, [{lname, _L2, Name}]}, Ctx) ->
    atext(Name, Ctx);
pp_rule_key({lqname, _L1, [{lname, _L2, var}, {lname, _L3, Name}]}, Ctx) ->
    beside(text("--"), atext(Name, Ctx));
pp_rule_key(Ast, Ctx) ->
    pp_unk("rule key", Ast, Ctx).

pp_rule_val({lqname, _L1, [{lname, _L2, Name}]}, Ctx) ->
    atext(Name, Ctx);
pp_rule_val({integer, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp_rule_val({float, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp_rule_val({string, _L1, V}, _Ctx) ->
    quote_string(V);
pp_rule_val({list, _L1, Items}, Ctx) ->
    sep([pp_rule_val(Item, Ctx) || Item <- Items]);
pp_rule_val({uop, _, Op, Val}, Ctx) ->
    beside(atext(Op, Ctx), pp_rule_val(Val, Ctx));
pp_rule_val({lqname, _L1, [{lname, _L2, var}, {lname, _L3, Name}]}, Ctx) ->
    besidel([text("var(--"), atext(Name, Ctx), text(")")], Ctx);
pp_rule_val({tagged, _L0, {{lqname, _L1, [{lname, _L2, hex}]}, {string, _l3, V}}},
            _Ctx) ->
    beside(text("#"), text(V));
pp_rule_val({tagged, _L0, {{lqname, _L1, [{lname, _L2, rgb}]}, {tuple, _l3, Colors}}},
            Ctx) ->
    besidel([text("rgb("), join(Colors, Ctx, fun pp_rule_val/2, text(",")), text(")")], Ctx);
pp_rule_val({tagged, _L0, {{lqname, _L1, [{lname, _L2, rgba}]}, {tuple, _l3, Colors}}},
            Ctx) ->
    besidel([text("rgba("), join(Colors, Ctx, fun pp_rule_val/2, text(",")), text(")")], Ctx);
pp_rule_val({tagged, _L0, {{lqname, _L1, [{lname, _L2, hsl}]}, {tuple, _l3, Colors}}},
            Ctx) ->
    besidel([text("hsl("), join_no_sep(Colors, Ctx, fun pp_rule_val/2, text(" ")), text(")")],
            Ctx);
pp_rule_val({tagged, _L0, {{lqname, _L1, [{lname, _L2, hsla}]}, {tuple, _l3, Colors}}},
            Ctx) ->
    besidel([text("hsla("),
             join_no_sep(Colors, Ctx, fun pp_rule_val/2, text(" ")),
             text(")")],
            Ctx);
pp_rule_val({tagged, _L0, {{lqname, _L1, [{lname, _L2, Tag}]}, Value}}, Ctx) ->
    case is_valid_value_unit(Tag) of
        {true, Unit} ->
            besidel([pp_rule_val(Value, Ctx), atext(Unit, Ctx)], Ctx);
        false ->
            sep([pp_rule_val(Value, Ctx), pp_unk("rule val tag", Tag, Ctx)])
    end;
pp_rule_val(Ast, Ctx) ->
    pp_unk("rule val", Ast, Ctx).

is_valid_value_unit(V = 'rem') ->
    {true, V};
is_valid_value_unit(V = em) ->
    {true, V};
is_valid_value_unit(V = pt) ->
    {true, V};
is_valid_value_unit(V = px) ->
    {true, V};
is_valid_value_unit(V = ch) ->
    {true, V};
is_valid_value_unit(pc) ->
    {true, '%'};
is_valid_value_unit(_) ->
    false.

pp_unk_if_not_empty_seq({seq, _, Items}, Ctx) ->
    pp_unk_if_not_empty(Items, Ctx).
