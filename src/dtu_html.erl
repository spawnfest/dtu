-module(dtu_html).

-export([format/1]).

-define(PAPER, 80).
-define(RIBBON, 56).

-import(prettypr, [text/1, sep/1, beside/2, empty/0]).
-import(dtu_pp,
        [abovel/2, besidel/2, pp_unk/3, atext/2, ntext/2, nestc/2, quote_string/1]).

format(RootNodes) ->
    format(RootNodes, ?PAPER, ?RIBBON).

format(RootNodes, Paper, Ribbon) ->
    prettypr:format(pp_tags(RootNodes, dtu_pp:new_ctx()), Paper, Ribbon).

pp_tags({seq, _L1, Nodes}, Ctx) ->
    pp_tags(Nodes, Ctx);
pp_tags(Nodes, Ctx) when is_list(Nodes) ->
    abovel([pp(Node, Ctx) || Node <- Nodes], Ctx);
pp_tags(Ast, Ctx) ->
    pp_unk("tags", Ast, Ctx).

pp({node, _L1, {{lqname, _L2, [{lname, _L3, Tag = style}]}, Attrs, {seq, _L4, Body}}},
   Ctx) ->
    abovel([besidel([text("<"), atext(Tag, Ctx), pp_attrs(Attrs, Ctx), text(">")], Ctx),
            nestc(dtu_css:pp_root(Body, Ctx), Ctx),
            besidel([text("</"), atext(Tag, Ctx), text(">")], Ctx)],
           Ctx);
pp({node, _L1, {{lqname, _L2, [{lname, _L3, Tag = script}]}, Attrs, {seq, _L4, Body}}},
   Ctx) ->
    abovel([besidel([text("<"), atext(Tag, Ctx), pp_attrs(Attrs, Ctx), text(">")], Ctx),
            nestc(dtu_js:pp_root(Body, Ctx), Ctx),
            besidel([text("</"), atext(Tag, Ctx), text(">")], Ctx)],
           Ctx);
pp({node, _L1, {{lqname, _L2, [{lname, _L3, Tag}]}, Attrs, Body}}, Ctx) ->
    case tag_body_is_empty_and_is_self_closing(Tag, Body) of
        true ->
            besidel([text("<"), atext(Tag, Ctx), pp_attrs(Attrs, Ctx), text("/>")], Ctx);
        false ->
            abovel([besidel([text("<"), atext(Tag, Ctx), pp_attrs(Attrs, Ctx), text(">")], Ctx),
                    nestc(pp_tags(Body, Ctx), Ctx),
                    besidel([text("</"), atext(Tag, Ctx), text(">")], Ctx)],
                   Ctx)
    end;
pp({string, _L1, V}, _Ctx) ->
    text(V);
pp({integer, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp({float, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp(Ast, Ctx) ->
    pp_unk("top", Ast, Ctx).

pp_attrs([], _Ctx) ->
    empty();
pp_attrs(Attrs, Ctx) ->
    beside(text(" "), sep([pp_attr(Attr, Ctx) || Attr <- Attrs])).

pp_attr({pair,
         _L1,
         {{lqname, _L2, [{lname, _L3, style}]}, {tagged, _, {_, {map, _, Rules}}}}},
        Ctx) ->
    Style =
        prettypr:format(
            dtu_css:pp_rules(Rules, Ctx), ?PAPER, ?RIBBON),
    StyleInline = string:replace(Style, "\n", " ", all),
    besidel([text("style="), quote_string(StyleInline)], Ctx);
pp_attr({pair, _L1, {Key, Val}}, Ctx) ->
    besidel([pp_attr_key(Key, Ctx), text("="), pp_attr_val(Val, Ctx)], Ctx);
pp_attr({lqname, _L2, [{lname, _L3, Name}]}, Ctx) ->
    atext(Name, Ctx);
pp_attr(Ast, Ctx) ->
    pp_unk("attr", Ast, Ctx).

pp_attr_key({lqname, _L1, [{lname, _L2, Name}]}, Ctx) ->
    atext(Name, Ctx);
pp_attr_key(Ast, Ctx) ->
    pp_unk("attr key", Ast, Ctx).

pp_attr_val({lqname, _L1, [{lname, _L2, Name}]}, _Ctx) ->
    quote_string(atom_to_list(Name));
pp_attr_val({string, _L1, V}, _Ctx) ->
    quote_string(V);
pp_attr_val({integer, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp_attr_val({float, _L1, V}, Ctx) ->
    ntext(V, Ctx);
pp_attr_val(Ast, Ctx) ->
    pp_unk("attr val", Ast, Ctx).

tag_body_is_empty_and_is_self_closing(Tag, {seq, _, []}) ->
    tag_is_self_closing(Tag);
tag_body_is_empty_and_is_self_closing(_, _) ->
    false.

tag_is_self_closing(area) ->
    true;
tag_is_self_closing(base) ->
    true;
tag_is_self_closing(br) ->
    true;
tag_is_self_closing(col) ->
    true;
tag_is_self_closing(embed) ->
    true;
tag_is_self_closing(hr) ->
    true;
tag_is_self_closing(img) ->
    true;
tag_is_self_closing(input) ->
    true;
tag_is_self_closing(link) ->
    true;
tag_is_self_closing(meta) ->
    true;
tag_is_self_closing(param) ->
    true;
tag_is_self_closing(source) ->
    true;
tag_is_self_closing(track) ->
    true;
tag_is_self_closing(_Tag) ->
    false.
