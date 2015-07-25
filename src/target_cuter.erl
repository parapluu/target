-module(target_cuter).
-behaviour(target_strategy).
-export([init_strategy/1,
	 init_target/1,
	 store_target/2,
	 update_target_state/2,
	 retrieve_target/1]).

init_strategy(Prop) ->
    %% TODO:
    %%   create random input to property
    %%   run cuter on generated input and property code
    %%   either return static failing or passing property
    %%   OR return property that uses cuter to explore one more path 
    %%   each test run
    undefined.

%% targets are not used in concolic testing
init_target(_) ->
    ok.

store_target(_, _) ->
    ok.

update_target_state(_, _) ->
    ok.

retrieve_target(Hash) ->
    ok.
