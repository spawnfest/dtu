-module(dtu_js).

-export([format/1, pp_root/2]).

-define(PAPER, 80).
-define(RIBBON, 56).

-import(prettypr, [text/1, beside/2, sep/1]).
-import(dtu_pp,
        [abovel/2,
         besidel/2,
         pp_unk/3,
         atext/2,
         ntext/2,
         nestc/2,
         join/4,
         join_no_sep/4,
         quote_string/1]).

format(RootNodes) ->
    format(RootNodes, ?PAPER, ?RIBBON).

format(RootNodes, Paper, Ribbon) ->
    prettypr:format(pp_root(RootNodes, dtu_pp:new_ctx()), Paper, Ribbon).

pp_root(Nodes, Ctx) ->
    abovel([pp_top_level(Node, Ctx) || Node <- Nodes], Ctx).

pp_top_level({node,
              _L1,
              {{lqname, _L2, [{lname, _, fn}, {lname, _L3, Name}]}, Args, {seq, _L4, Exprs}}},
             Ctx) ->
    abovel([besidel([text("function "),
                     atext(Name, Ctx),
                     text("("),
                     pp_fn_args(Args, Ctx),
                     text(") {")],
                    Ctx),
            nestc(pp_body_exprs(Exprs, Ctx), Ctx),
            text("}")],
           Ctx);
pp_top_level(Ast, Ctx) ->
    pp_expr(Ast, Ctx).

pp_body_exprs(Exprs, Ctx) ->
    abovel([beside(pp_body_expr(Expr, Ctx), text(";")) || Expr <- Exprs], Ctx).

pp_body_expr(Expr, Ctx) ->
    pp_expr(Expr, Ctx).

pp_expr({node, _L1, {{lqname, _L2, [{lname, _L3, return}]}, [Arg], {seq, _L4, []}}},
        Ctx) ->
    besidel([text("return "), pp_expr(Arg, Ctx)], Ctx);
pp_expr({node, _L1, {{lqname, _L2, [{lname, _L3, 'let'}]}, [], {seq, _L4, Assignments}}},
        Ctx) ->
    besidel([text("let "), join(Assignments, Ctx, fun pp_assign/2, text(","))], Ctx);
pp_expr({node, _L1, {{lqname, _L2, [{lname, _L3, 'if'}]}, Cond, {seq, _L4, Body}}},
        Ctx) ->
    abovel([
    besidel([text("if ("), join(Cond, Ctx, fun pp_expr/2, text(",")), text(") {")], Ctx),
    nestc(pp_body_exprs(Body, Ctx), Ctx),
    text("}")], Ctx);
pp_expr({node, _L1, {{lqname, _L2, Path}, Args, {seq, _L4, []}}}, Ctx) ->
    besidel([pp_path(Path, Ctx), text("("), pp_call_args(Args, Ctx), text(")")], Ctx);
pp_expr(Expr, Ctx) ->
    pp_value(Expr, Ctx).

pp_assign({pair, _, {Name, Expr}}, Ctx) ->
    besidel([pp_lrs(Name, Ctx), text(" = "), pp_expr(Expr, Ctx)], Ctx);
pp_assign(Ast, Ctx) ->
    pp_unk("assign", Ast, Ctx).

pp_lrs({lqname, _, [{lname, _, Name}]}, Ctx) ->
    atext(Name, Ctx);
pp_lrs(Ast, Ctx) ->
    pp_unk("lrs", Ast, Ctx).

pp_fn_args(Args, Ctx) ->
    pp_seq_items(Args, Ctx).

pp_call_args(Args, Ctx) ->
    pp_seq_items(Args, Ctx).

pp_value({lqname, _L2, Path}, Ctx) ->
    pp_path(Path, Ctx);
pp_value({string, _L1, V}, _Ctx) ->
    quote_string(V);
pp_value({integer, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp_value({float, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp_value({list, _L1, V}, Ctx) ->
    pp_seq("[", "]", V, Ctx);
pp_value({map, _L1, V}, Ctx) ->
    pp_seq("{", "}", V, Ctx);
pp_value({expr, _L1, {Left, Op, Right}}, Ctx) ->
    sep([pp_expr(Left, Ctx), atext(Op, Ctx), pp_expr(Right, Ctx)]);
pp_value({uop, _, {Op, V}}, Ctx) ->
    beside(atext(Op, Ctx), pp_value(V, Ctx));
pp_value({node, _, {{lqname, _, [{lname, _, fn}]}, Args, {seq, _, Exprs}}}, Ctx) ->
    abovel([besidel([text("("), pp_fn_args(Args, Ctx), text(") => {")], Ctx),
            pp_body_exprs(Exprs, Ctx),
            text("}")],
           Ctx);
pp_value(Ast, Ctx) ->
    pp_unk("value", Ast, Ctx).

pp_obj_key({lqname, _L2, [{lname, _L3, Name}]}, _Ctx) ->
    quote_string(atom_to_list(Name));
pp_obj_key(Ast, Ctx) ->
    pp_value(Ast, Ctx).

pp_obj_val(Ast, Ctx) ->
    pp_value(Ast, Ctx).

pp_seq_item({uop, _, {'*', {lqname, _, [{lname, _, Name}]}}}, Ctx) ->
    beside(text("..."), atext(Name, Ctx));
pp_seq_item({pair, _, {Key, Val}}, Ctx) ->
    besidel([pp_obj_key(Key, Ctx), text(": "), pp_obj_val(Val, Ctx)], Ctx);
pp_seq_item(Ast, Ctx) ->
    pp_expr(Ast, Ctx).

pp_seq(Open, Close, Items, Ctx) ->
    besidel([text(Open), pp_seq_items(Items, Ctx), text(Close)], Ctx).

pp_seq_items(Items, Ctx) ->
    join(Items, Ctx, fun pp_seq_item/2, text(",")).

pp_path(Path, Ctx) ->
    join_no_sep(Path, Ctx, fun pp_path_item/2, text(".")).

pp_path_item({lname, _, Name}, Ctx) ->
    atext(Name, Ctx);
pp_path_item({uname, _, Name}, Ctx) ->
    atext(Name, Ctx);
pp_path_item(Ast, Ctx) ->
    pp_unk("path item", Ast, Ctx).
