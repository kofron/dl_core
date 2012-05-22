all: compile test

compile: get-deps
	@(./rebar compile)

get-deps: 
	@(./rebar get-deps)

release: clean compile
	@(./rebar generate)

clean:
	@(./rebar clean)
	rm -Rf release/dripline

test:
	@(./rebar skip_deps=true eunit)