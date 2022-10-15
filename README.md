# dtu

## Format

### Literals

```clojure
42
42.5
"string"
```

### Tagged

```clojure
#ts 42
#kg 42.5
#re "string"
```

### Names

```clojure
foo
foo.bar
foo/bar
foo.bar/baz
foo.bar/baz.argh
```

### Collections

```clojure
#[]
#()
#{}
#[1]
#(1)
#{1}
#[1, 2]
#(1, 2)
#{1, 2}
#[1,]
#(1,)
#{1,}
#[1, #{2, #()}, foo]
```

### Nodes

```clojure
foo ()
foo () {}
foo {}
foo (hello)
foo (42) {"hi"}
foo {42}
foo (a: 42) {b: 42}
```

#### Alt Body

```clojure
cond { | 1 }
cond { | true: 1 }
cond { | true: 1 | false: 0 }
```

### Expressions

No operator precedence, use parenthesis

```clojure
1 + 2
1 + 2 * 3
1 + (2 * 3)
1 + 2 +- #ts 32
```

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/dtu
