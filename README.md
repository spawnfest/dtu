# dtu: Data The Ultimate

DTU is a data format that, unlike YAML, allows to define pretty decent
programming language.

🎥 Watch overview video here 👇

[![DTU Explanation Youtube Cover](https://raw.githubusercontent.com/spawnfest/dtu/main/resources/dtu-yt-cover.png)](https://www.youtube.com/watch?v=ZE3KXGSp8WE)

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

![TodoMVC.dtu Screenshot](https://raw.githubusercontent.com/spawnfest/dtu/main/resources/todomvc-dtu.png)

To drive the testing and feature selection for each dialect I started implemented
[TodoMVC](https://todomvc.com/), you can read the frontend code here [todomvc.dtu](https://github.com/spawnfest/dtu/blob/main/examples/todomvc.dtu).

You can try it here: [TodoMVC DTU demo](http://marianoguerra.github.io/experiments/todomvc.html)

You may notice that the javascript and css are inline in their own dialects, DTU allows
you to switch dialects in the same file and do the right thing.

For example, [the `style` node](https://github.com/spawnfest/dtu/blob/9e7fadaac668e1461c76758d977334955fafa1b6/examples/todomvc.dtu#L5) will [render the CSS in the body like here](https://github.com/spawnfest/dtu/blob/9e7fadaac668e1461c76758d977334955fafa1b6/todomvc.html#L7).

The CSS in [the `style` attribute](https://github.com/spawnfest/dtu/blob/9e7fadaac668e1461c76758d977334955fafa1b6/examples/html.dtu#L22) will [render it inline as a string like here](https://github.com/spawnfest/dtu/blob/2e16608743ab93d02b0411c36dd22a92c4ac9d16/examples/dtu.html#L23).

> **Note**
> The whole TodoMVC is defined in a single file, embedding [HTML](https://github.com/spawnfest/dtu/blob/main/examples/todomvc.dtu), [CSS]( https://github.com/spawnfest/dtu/blob/2e16608743ab93d02b0411c36dd22a92c4ac9d16/examples/todomvc.dtu#L5) and [JS](https://github.com/spawnfest/dtu/blob/2e16608743ab93d02b0411c36dd22a92c4ac9d16/examples/todomvc.dtu#L109)

As I achieved the objective I went for a stretch and started implementing a backend for
it in a dialect of [Erlang](https://github.com/spawnfest/dtu/blob/main/examples/erlang.dtu).

This one embeds HTML, CSS and JS [inside Erlang as seen here](https://github.com/spawnfest/dtu/blob/2e16608743ab93d02b0411c36dd22a92c4ac9d16/examples/erlang.dtu#L24), in this case
the content is rendered as a string to be served as a response to a `GET /` request,
you can [see the result here](https://github.com/spawnfest/dtu/blob/2e16608743ab93d02b0411c36dd22a92c4ac9d16/dtu_backend.erl#L20).

## Usage

To run the server, first compile dtu:

```sh
rebar3 compile
rebar3 escriptize
mv _build/default/bin/dtu .
```

Then translate DTU to Erlang:

```sh
./dtu erl examples/erlang.dtu | tee dtu_backend.erl
```

Then start a shell to run the server:

```sh
rebar3 as test shell
```

Compile the module and start the server:

```erlang
c(dtu_backend).
{ok, Pid} = elli:start_link([{callback, dtu_backend}, {port, 3000}]).
```

And try it with curl:

```sh
curl -v http://localhost:3000/hello/world
# 200
# Hello World!
curl -v http://localhost:3000/hello/notfound
# 404
# Not Found
curl -v http://localhost:3000/task -d "name=do stuff"
# 201
# Created
```

If you open [localhost:3000](http://localhost:3000/) in your browser the app will load.

## Format

To get a quick idea of the possibilities of the format check [erlang.dtu](https://github.com/spawnfest/dtu/blob/main/examples/erlang.dtu).

### Literals

```clojure
42
42.5
"string"
```

### Tagged

Any name can be used as a tag, the meaning is given by the translator

```clojure
#ts 42
#kg 42.5
#iso.units@weight.kg 42.5
#re "string"
#set {1, 1, 2, 3}
#date "2022-10-16T04:39:34Z"
```

### Names

```clojure
foo
foo.bar
foo@bar
foo.Bar@baz
foo.bar@Baz.Argh
```

### Collections

Collections by themselves don't have any meaning, this are the default names.

you may notice that all collections can have individual values and key/value
pairs, it's the task of the translator to decide which values are valid.

#### List

```clojure
#[]
#[1]
#[1, 2]
#[1, answer: 42]
#[1, #{2, #()}, foo]
```

#### Tuple

```clojure
#()
#(1)
#(1, 2)
#(1, answer: 42)
#(1, #{2, #()}, foo)
```
#### Map

```clojure
#{}
#{1}
#{1, 2}
#{1, answer: 42}
#{1, #{2, #()}, foo}
```

### Nodes

Nodes are what makes DTU better at describing logic than YAML and JSON, a node is a name
followed optionally by a head `(...)` and optionally by a body `{...}`.

Like with collections any value that can be in a sequence can be in the head or
the body of a node.

```clojure
foo ()
foo.bar () {}
foo@bar {}
foo.bar@baz (hello)
foo.bar@baz.argh (42) {"hi"}
foo {42}
foo (a: 42) {b: 42}
```

#### Alt Body

The body of a node can also be an `Alt Body` which is syntax to specify alternatives.

```clojure
cond {
    | true: 1
    | false: 0
}

switch (name) {
    | "bob": "yellow"
    | "patrick": "pink"
}

type (Bool) {
    | True
    | False
}
```

### Expressions

No operator precedence, use parenthesis.

Any token starting with one of `< = > ! % & ? * - + / ~` is a symbol, symbols
have no specific meaning.

```clojure
1 + 2
1 + 2 * 3
1 + (2 * 3)
1 + 2 +- #ts 32
```

## Code Index

- [dtu.erl](https://github.com/spawnfest/dtu/blob/main/src/dtu.erl): escript entry point

- [dtu_lexer.xrl](https://github.com/spawnfest/dtu/blob/main/src/dtu_lexer.xrl): Lexer
- [dtu_parser.yrl](https://github.com/spawnfest/dtu/blob/main/src/dtu_parser.yrl): Parser

- [dtu_html.erl](https://github.com/spawnfest/dtu/blob/main/src/dtu_html.erl): HTML translator
- [dtu_css.erl](https://github.com/spawnfest/dtu/blob/main/src/dtu_css.erl): CSS translator
- [dtu_js.erl](https://github.com/spawnfest/dtu/blob/main/src/dtu_js.erl): JS translator
- [dtu_erl.erl](https://github.com/spawnfest/dtu/blob/main/src/dtu_erl.erl): Erlang translator

- [dtu_pp.erl](https://github.com/spawnfest/dtu/blob/main/src/dtu_pp.erl): translator utilities

## License

MIT
