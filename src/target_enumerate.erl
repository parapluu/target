-module(target_enumerate).
-behaviour(target_strategy).
-export([init_strategy/1,
	 init_target/1,
	 store_target/2,
	 retrieve_target/1,
	 update_global_fitness/1,
	 get_shrinker/1
	]).

init_strategy(Prop) ->
    erase(target_enumerate_data),
    undefined = put(target_enumerate_data, dict:new()),
    Prop.

init_target(_) ->
    %% enumerate targets are all integer targets counting from 0 upwards
    {0, fun (Last) -> {Last+1, Last+1} end, fun (TargetState, _Fitness) -> TargetState end}.

store_target(Key, Target) ->
    NewDict = dict:store(Key, Target, get(target_enumerate_data)),
    put(target_enumerate_data, NewDict),
    ok.

retrieve_target(Key) ->
    Dict = get(target_enumerate_data),
    case dict:is_key(Key, Dict) of
	true ->
	    dict:fetch(Key, Dict);
	false ->
	    undefined
    end.

update_global_fitness(_Fitness) ->
    ok.

get_shrinker(_) ->
    proper_types:integer().
