-module(target_enumerate).
-behaviour(target_strategy).
-export([init_strategy/0,
	 init_target/1,
	 store_target/2,
	 update_target_state/2,
	 retrieve_target/1]).

init_strategy() ->
    io:format("INIT~n"),
    erase(target_enumerate_data),
    undefined = put(target_enumerate_data, dict:new()),
    ok.

init_target(_) ->
    %% enumerate targets are all integer targets counting from 0 upwards
    {0, fun (Last) -> {Last+1, Last+1} end, fun (_,_) -> ok end}.

store_target(Hash, Target) ->
    NewDict = dict:store(Hash, Target, get(target_enumerate_data)),
    put(target_enumerate_data, NewDict),
    ok.

update_target_state(Hash, State) ->
    {_, F1, F2} = retrieve_target(Hash),
    NewDict = dict:store(Hash, {State, F1, F2}, get(target_enumerate_data)),
    put(target_enumerate_data, NewDict),
    ok.

retrieve_target(Hash) ->
    Dict = get(target_enumerate_data),
    case dict:is_key(Hash, Dict) of
	true ->
	    dict:fetch(Hash, get(target_enumerate_data));
	false ->
	    undefined
    end.

