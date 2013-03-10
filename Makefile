all: compile test

compile: get-deps
	@(./rebar compile)

get-deps: 
	@(./rebar get-deps)

release: clean compile
	@(./rebar generate)

clean:
	@(./rebar clean)

test:
	@(./rebar skip_deps=true eunit)
	@(./rebar skip_deps=true ct)