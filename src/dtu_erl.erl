-module(dtu_erl).

-export([format/1, format/2, pp_root/2]).

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
         quote_string/1,
         quote_string/2]).

format(RootNodes) ->
    format(RootNodes, dtu_pp:new_ctx()).

format(RootNodes, Ctx) ->
    format(RootNodes, Ctx, ?PAPER, ?RIBBON).

format(RootNodes, Ctx, Paper, Ribbon) ->
    prettypr:format(pp_root(RootNodes, Ctx), Paper, Ribbon).

pp_root(Nodes, Ctx) ->
    abovel([beside(pp_tl(Node, Ctx), text("\n")) || Node <- Nodes], Ctx).

pp_tl({node, _, {{lqname, _, [{lname, _, fn}, {lname, _, FName}]}, [], {seq, _, Body}}},
      Ctx) ->
    beside(join(Body, Ctx, fun(Ast, Ctx1) -> pp_fn_clause(FName, Ast, Ctx1) end, text(";")),
           text("."));
pp_tl({node,
       _,
       {{lqname, _, [{lname, _, fn}, {lname, _, FName}]}, Args = [_ | _], {seq, _, Body}}},
      Ctx) ->
    abovel([besidel([atext(FName, Ctx), text("("), pp_fn_args(Args, Ctx), text(") ->")], Ctx),
            nestc(beside(pp_body_exprs(Body, Ctx), text(".")), Ctx)],
           Ctx);
pp_tl({node,
       _,
       {{lqname, _, [{lname, _, module}]}, [{lqname, _, [{lname, _, ModName}]}], {seq, _, []}}},
      Ctx) ->
    besidel([text("-module("), atext(ModName, Ctx), text(").")], Ctx);
pp_tl({node,
       _,
       {{lqname, _, [{lname, _, behaviour}]},
        [{lqname, _, [{lname, _, ModName}]}],
        {seq, _, []}}},
      Ctx) ->
    besidel([text("-behaviour("), atext(ModName, Ctx), text(").")], Ctx);
pp_tl({node, _, {{lqname, _, [{lname, _, export}]}, FnRefs, {seq, _, []}}}, Ctx) ->
    besidel([text("-export(["), pp_attr_fn_refs(FnRefs, Ctx), text("]).")], Ctx);
pp_tl({node,
       _,
       {{lqname, _, [{lname, _, include_lib}]}, [{string, _, Path}], {seq, _, []}}},
      Ctx) ->
    besidel([text("-include_lib("), quote_string(Path), text(").")], Ctx);
pp_tl(Ast, Ctx) ->
    pp_unk("top", Ast, Ctx).

pp_attr_fn_refs(FnRefs, Ctx) ->
    join(FnRefs, Ctx, fun pp_attr_fn_ref/2, text(",")).

pp_attr_fn_ref({node,
                _,
                {{lqname, _, [{lname, _, fn}, {lname, _, FnName}]},
                 [{integer, _, Arity}],
                 {seq, _, []}}},
               Ctx) ->
    besidel([atext(FnName, Ctx), text("/"), ntext(Arity, Ctx)], Ctx);
pp_attr_fn_ref(Ast, Ctx) ->
    pp_unk("fn ref", Ast, Ctx).

pp_fn_clause(FName,
             {node, _, {{lqname, _, [{lname, _, 'case'}]}, Args, {seq, _, Body}}},
             Ctx) ->
    abovel([besidel([atext(FName, Ctx), text("("), pp_fn_args(Args, Ctx), text(") ->")], Ctx),
            nestc(pp_body_exprs(Body, Ctx), Ctx)],
           Ctx);
pp_fn_clause(_FName, Ast, Ctx) ->
    pp_unk("fn clause", Ast, Ctx).

pp_fn_args(Args, Ctx) ->
    join(Args, Ctx, fun pp_fn_arg/2, text(",")).

pp_fn_arg(Ast, Ctx) ->
    pp_expr(Ast, Ctx).

pp_call_args(Args, Ctx) ->
    join(Args, Ctx, fun pp_call_arg/2, text(",")).

pp_call_arg(Ast, Ctx) ->
    pp_expr(Ast, Ctx).

pp_body_exprs(Args, Ctx) ->
    join(Args, Ctx, fun pp_body_expr/2, text(",")).

pp_body_expr(Ast, Ctx) ->
    pp_expr(Ast, Ctx).

pp_expr({integer, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp_expr({float, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp_expr({string, _L1, V}, Ctx) ->
    besidel([text("<<"), quote_string(V), text(">>")], Ctx);
pp_expr({tagged, _, {{lqname, _, [{lname, _, s}]}, {string, _L1, V}}}, _Ctx) ->
    quote_string(V);
pp_expr({list, _L1, V}, Ctx) ->
    pp_seq("[", "]", V, Ctx);
pp_expr({tuple, _L1, V}, Ctx) ->
    pp_seq("{", "}", V, Ctx);
pp_expr({map, _L1, V}, Ctx) ->
    pp_seq("#{", "}", V, Ctx);
pp_expr({lqname, _, [{uname, _, Name}]}, Ctx) ->
    atext(Name, Ctx);
pp_expr({lqname, _, [{lname, _, Name}]}, Ctx) ->
    atext(Name, Ctx);
pp_expr({lqname, _, [{lname, _, a}, {uname, _, Name}]}, _Ctx) ->
    quote_string(atom_to_list(Name), $');
pp_expr({pair, _, {LHS, Expr}}, Ctx) ->
    besidel([pp_lhs(LHS, Ctx), text(" = "), pp_expr(Expr, Ctx)], Ctx);
% r.Req@req.method
pp_expr({fqname,
         _,
         {[{lname, _, r}, {uname, _, VarName}], [{lname, _, RecordName}, {lname, _, FieldName}]}},
        Ctx) ->
    besidel([atext(VarName, Ctx),
             text("#"),
             atext(RecordName, Ctx),
             text("."),
             atext(FieldName, Ctx)],
            Ctx);
pp_expr({node, _, {{lqname, _, [{lname, _, Name}]}, Args, {seq, _, []}}}, Ctx) ->
    besidel([atext(Name, Ctx), text("("), pp_call_args(Args, Ctx), text(")")], Ctx);
pp_expr({node,
         _,
         {{lqname, _, [{lname, _, MName}, {lname, _, FName}]}, Args, {seq, _, []}}},
        Ctx) ->
    besidel([atext(MName, Ctx),
             text(":"),
             atext(FName, Ctx),
             text("("),
             pp_call_args(Args, Ctx),
             text(")")],
            Ctx);
pp_expr(Ast, Ctx) ->
    pp_unk("expr", Ast, Ctx).

pp_lhs(Ast, Ctx) ->
    pp_expr(Ast, Ctx).

pp_seq_item(Ast, Ctx) ->
    pp_expr(Ast, Ctx).

pp_seq(Open, Close, Items, Ctx) ->
    besidel([text(Open), pp_seq_items(Items, Ctx), text(Close)], Ctx).

pp_seq_items(Items, Ctx) ->
    join(Items, Ctx, fun pp_seq_item/2, text(",")).
