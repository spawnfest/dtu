# dtu: Data The Ultimate

DTU is a data format that, unlike YAML, allows to define pretty decent
programming language.

## Implementation

DTU is implemented using:

- [leex](https://www.erlang.org/doc/man/leex.html) for lexing
- [yecc](https://www.erlang.org/doc/man/yecc.html) for parsing
- [prettypr](https://www.erlang.org/doc/man/prettypr.html) for pretty printing
- [erlang](https://www.erlang.org/) for coding
- [rebar3](http://rebar3.org/) for the project
- [rebar3_format](https://github.com/AdRoll/rebar3_format) for code formatting
- [elli](https://github.com/elli-lib/elli) as the webserver for the example
- [just](https://github.com/casey/just) as command runner

## Dialects

To evaluate the capabilities of the format I implemented "translators" from DTU to the
following languages:

- CSS
  - status: pretty complete
  - example: [css.dtu](https://github.com/spawnfest/dtu/blob/main/examples/css.dtu)
- HTML
  - status: pretty complete
  - example: [html.dtu](https://github.com/spawnfest/dtu/blob/main/examples/html.dtu)
- Javascript
  - status: only the features I needed for TodoMVC
  - example: [js.dtu](https://github.com/spawnfest/dtu/blob/main/examples/js.dtu)
- Erlang
  - status: only the features I needed for TodoMVC
  - example: [erlang.dtu](https://github.com/spawnfest/dtu/blob/main/examples/erlang.dtu)

## TodoMVC

To drive the testing and feature selection for each dialect I started implemented
[TodoMVC](https://todomvc.com/), you can read the frontend code here [todomvc.dtu](https://github.com/spawnfest/dtu/blob/main/examples/todomvc.dtu).

You may notice that the javascript and css are inline in their own dialects, DTU allows
you to switch dialects in the same file and do the right thing.

For example, [the `style` node](https://github.com/spawnfest/dtu/blob/9e7fadaac668e1461c76758d977334955fafa1b6/examples/todomvc.dtu#L5) will [render the CSS in the body like here](https://github.com/spawnfest/dtu/blob/9e7fadaac668e1461c76758d977334955fafa1b6/todomvc.html#L7).

The CSS in [the `style` attribute](https://github.com/spawnfest/dtu/blob/9e7fadaac668e1461c76758d977334955fafa1b6/examples/html.dtu#L22) will render it inline as a string like here.


## Usage

```sh
just format-erl
rebar3 as test shell
```

```erlang
c(dtu_backend).
{ok, Pid} = elli:start_link([{callback, dtu_backend}, {port, 3000}]).
```

```sh
curl -v http://localhost:3000/hello/world
# 200
# Hello World!
curl http://localhost:3000/hello/notfound
# 404
# Not Found
curl -v http://localhost:3000/task -d "name=do stuff"
# 201
# Created
```


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
