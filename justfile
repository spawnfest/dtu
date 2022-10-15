
build: format
    rebar3 escriptize
    mv _build/default/bin/dtu .

format:
    rebar3 format
