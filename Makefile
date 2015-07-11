.PHONY: default fast all get-deps compile # dialyzer tests clean mrproper

ERL_INCLUDE = $(PWD):$(ERL_LIBS)

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

REBAR := .$(SEP)rebar

default: fast

fast: get-deps compile

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

doc:
	$(REBAR) doc skip_deps=true

clean:
	$(REBAR) clean

tests: compile
	ERL_LIBS=$(ERL_INCLUDE) $(REBAR) eunit skip_deps=true
