
build:
    rebar3 compile
    rebar3 escriptize
    mv _build/default/bin/dtu .

format:
    rebar3 format

smoke-test: build
    ./dtu "42" 
    ./dtu "42.5" 
    ./dtu '"string"'
    ./dtu "#ts 42" 
    ./dtu "#kg 42.5" 
    ./dtu '#re "string"'
    ./dtu "foo" 
    ./dtu "foo.bar" 
    ./dtu "foo/bar" 
    ./dtu "foo.bar/baz" 
    ./dtu "foo.bar/baz.argh" 
