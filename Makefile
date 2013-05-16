.PHONY: all
all: deps compile eunit rel

.PHONY: deps
deps:
	./rebar get-deps

.PHONY: compile
compile:
	./rebar compile

.PHONY: eunit
eunit:
	./rebar eunit skip_deps=true

.PHONY: clean
clean:
	./rebar clean

.PHONY: rel
rel:
	cd rel && ../rebar generate
