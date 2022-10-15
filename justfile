
build:
    rebar3 compile
    rebar3 escriptize
    mv _build/default/bin/dtu .

format:
    rebar3 format

format-css: build
    ./dtu css examples/css.dtu

format-html: build
    ./dtu html examples/html.dtu

smoke-test: build test-expr
    ./dtu "42"
    ./dtu "42.5"
    ./dtu '"string"'
    ./dtu "#ts 42"
    ./dtu "#kg 42.5"
    ./dtu '#re "string"'
    ./dtu "foo"
    ./dtu "max-width"
    ./dtu "foo.bar"
    ./dtu "foo@bar"
    ./dtu "foo.bar@baz"
    ./dtu "foo.bar@baz.argh"
    ./dtu "#[]"
    ./dtu "#()"
    ./dtu "#{}"

    ./dtu "#[1]"
    ./dtu "#(1)"
    ./dtu "#{1}"

    ./dtu "#[1, 2]"
    ./dtu "#(1, 2)"
    ./dtu "#{1, 2}"

    ./dtu "#[1,]"
    ./dtu "#(1,)"
    ./dtu "#{1,}"

    ./dtu "#[1, #{2, #()}, foo]"

    ./dtu "foo ()"
    ./dtu "foo () {}"
    ./dtu "foo {}"

    ./dtu "foo (hello)"
    ./dtu 'foo (42) {"hi"}'
    ./dtu "foo {42}"

    ./dtu "foo (a: 42) {b: 42}"

test-expr:
    ./dtu "a"
    ./dtu "-a"
    ./dtu "a + b"
    ./dtu "a + -b"
    ./dtu "-42"
    ./dtu '-1 + -2'
    ./dtu '1 + 2'
    ./dtu '1 / 2'
    ./dtu '1 ~ 2'
    ./dtu '1 + 2 * 3'
    ./dtu '1 + (2 * 3)'
    ./dtu '1 + 2 +- #ts 32'

test-alt-seq:
    ./dtu 'cond { | 1 }'
    ./dtu 'cond { | true: 1 }'
    ./dtu 'cond { | true: 1 | false: 0 }'

test-css:
    ./dtu 'style { All { box-sizing: border-box }, body { max-width: #ch 100, padding-left: 0, text-align: center, } }'
