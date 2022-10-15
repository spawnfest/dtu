Nonterminals
    doc tl_exprs tl_expr 
    tagged scalar
    node qname qname_items qname_item.

Terminals
    integer float string 
    lname uname 
    slash dot hash.

Rootsymbol
    doc.

doc -> tl_exprs : '$1'.

tl_exprs -> tl_expr : ['$1'].
tl_exprs -> tl_expr tl_exprs : ['$1'|'$2'].

tl_expr -> node : '$1'.

node -> qname  : '$1'.
node -> tagged : '$1'.

tagged -> hash qname scalar : {tagged, line('$1'), {'$2', '$3'}}.
tagged -> scalar : '$1'.

scalar -> integer : '$1'.
scalar -> float   : '$1'.
scalar -> string  : '$1'.

qname -> qname_items slash qname_items: {fqname, line('$1'), {'$1', '$3'}}.
qname -> qname_items : {lqname, line('$1'), '$1'}.

qname_items -> qname_item : ['$1'].
qname_items -> qname_item dot qname_items: ['$1' | '$3'].

qname_item -> lname : '$1'.
qname_item -> uname : '$1'.

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> line(H).
