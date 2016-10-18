.PHONY: default all get-deps compile dialyzer tests clean

ERL_INCLUDE = $(PWD):$(ERL_LIBS)

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

REBAR := .$(SEP)rebar

default: compile

all: get-deps compile tests dialyzer

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

dialyzer: .plt/proper_plt compile
	dialyzer -n --plt $< ebin -Wunmatched_returns -Wunderspecs

.plt/proper_plt: .plt
	dialyzer --build_plt --output_plt $@ --apps erts kernel stdlib compiler crypto syntax_tools deps/proper/ebin/*.beam

tests: compile
	ERL_LIBS=$(ERL_INCLUDE) $(REBAR) eunit skip_deps=true

doc:
	$(REBAR) doc skip_deps=true

clean:
	$(REBAR) clean
