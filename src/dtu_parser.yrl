Nonterminals
    doc tl_exprs tl_expr 
    tagged scalar pair
    collection list map tuple seq alt_seq
    node head body
    expr
    qname qname_items qname_item.

Terminals
    integer float string 
    lname uname 
    open close open_list close_list open_map close_map
    slash colon pipe dot hash sep symbol.

Rootsymbol
    doc.

doc -> tl_exprs : '$1'.

tl_exprs -> tl_expr : ['$1'].
tl_exprs -> tl_expr tl_exprs : ['$1'|'$2'].

tl_expr -> node : '$1'.

node -> qname           : '$1'.
node -> qname head body : {node, line('$1'), {'$1', '$2', '$3'}}.
node -> qname head      : {node, line('$1'), {'$1', '$2', {seq, line('$1'), []}}}.
node -> qname body      : {node, line('$1'), {'$1', [], '$2'}}.
node -> expr            : '$1'.

head -> open close      : [].
head -> open seq close  : '$2'.

body -> open_map close_map                  : {seq, line('$1'), []}.
body -> open_map seq close_map              : {seq, line('$1'), '$2'}.
body -> open_map pipe alt_seq close_map     : {alt_seq, line('$1'), '$3'}.

tagged -> hash collection           : '$2'.
tagged -> hash qname collection     : {tagged, line('$1'), {'$2', '$3'}}.
tagged -> hash qname scalar         : {tagged, line('$1'), {'$2', '$3'}}.
tagged -> pair                      : '$1'.

collection -> list  : '$1'.
collection -> map   : '$1'.
collection -> tuple : '$1'.

list -> open_list close_list           : {list, line('$1'), []}.
list -> open_list seq close_list       : {list, line('$1'), '$2'}.

map -> open_map close_map              : {map, line('$1'), []}.
map -> open_map seq close_map          : {map, line('$1'), '$2'}.

tuple -> open close                    : {tuple, line('$1'), []}.
tuple -> open seq close                : {tuple, line('$1'), '$2'}.

seq -> node : ['$1'].
seq -> node sep : ['$1'].
seq -> node sep seq : ['$1' | '$3'].

alt_seq -> expr : ['$1'].
alt_seq -> expr pipe alt_seq : ['$1' | '$3'].

pair -> scalar colon expr: {pair, line('$1'), {'$1', '$3'}}.
pair -> qname colon expr:  {pair, line('$1'), {'$1', '$3'}}.
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

expr -> tagged : '$1'.
expr -> symbol tagged : {uop, line('$1'), '$2'}.
expr -> expr symbol tagged : {expr, line('$1'), {'$1', unwrap('$2'), '$3'}}.
expr -> expr symbol symbol tagged : {expr, line('$1'), {'$1', unwrap('$2'), {uop, line('$3'), '$4'}}}.
expr -> expr symbol open expr close: {expr, line('$1'), {'$1', unwrap('$2'), '$4'}}.

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> line(H).
