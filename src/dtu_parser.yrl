Nonterminals
    doc tl_exprs tl_expr.

Terminals
    integer.

Rootsymbol
    doc .

doc -> tl_exprs : '$1'.

tl_exprs -> tl_expr : ['$1'].
tl_exprs -> tl_expr tl_exprs : ['$1'|'$2'].

tl_expr -> integer : '$1'.
