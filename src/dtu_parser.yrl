Nonterminals
    doc tl_exprs tl_expr 
    tagged data_literal scalar pair
    collection list map tuple seq
    node head body
    qname qname_items qname_item.

Terminals
    integer float string 
    lname uname 
    open close open_list close_list open_map close_map
    slash colon dot hash sep.

Rootsymbol
    doc.

doc -> tl_exprs : '$1'.

tl_exprs -> tl_expr : ['$1'].
tl_exprs -> tl_expr tl_exprs : ['$1'|'$2'].

tl_expr -> node : '$1'.

node -> qname  : '$1'.
node -> qname head body : {node, line('$1'), {'$1', '$2', '$3'}}.
node -> qname head : {node, line('$1'), {'$1', '$2', []}}.
node -> qname body : {node, line('$1'), {'$1', [], '$2'}}.
node -> tagged : '$1'.

head -> open close          : [].
head -> open seq close      : '$2'.

body -> open_map close_map       : [].
body -> open_map seq close_map   : '$2'.

tagged -> hash qname data_literal: {tagged, line('$1'), {'$2', '$3'}}.
tagged -> data_literal : '$1'.

data_literal -> collection : '$1'.
data_literal -> pair       : '$1'.

collection -> list  : '$1'.
collection -> map   : '$1'.
collection -> tuple : '$1'.

list -> hash open_list close_list           : {list, line('$1'), []}.
list -> hash open_list seq close_list       : {list, line('$1'), '$3'}.

map -> hash open_map close_map              : {map, line('$1'), []}.
map -> hash open_map seq close_map          : {map, line('$1'), '$3'}.

tuple -> hash open close                    : {tuple, line('$1'), []}.
tuple -> hash open seq close                : {tuple, line('$1'), '$3'}.

seq -> node : ['$1'].
seq -> node sep : ['$1'].
seq -> node sep seq : ['$1' | '$3'].

pair -> scalar colon scalar : {pair, line('$1'), {'$1', '$3'}}.
pair -> qname  colon scalar : {pair, line('$1'), {'$1', '$3'}}.
pair -> scalar : '$1'.

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
