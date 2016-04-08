-module(target_hillclimb).
-behaviour(target_strategy).
-export([init_strategy/1,
         init_target/1,
         store_target/2,
         retrieve_target/1,
         update_global_fitness/1,
         get_shrinker/1
        ]).

%% init functions
init_strategy(Prop) ->
    erase(target_hillclimb_data),
    undefined = put(target_hillclimb_data, dict:new()),
    Prop.

%% storage functions
store_target(Key, Target) ->
    NewDict = dict:store(Key, Target, get(target_hillclimb_data)),
    put(target_hillclimb_data, NewDict),
    ok.

retrieve_target(Key) ->
    Dict = get(target_hillclimb_data),
    case dict:is_key(Key, Dict) of
        true ->
            dict:fetch(Key, Dict);
        false ->
            undefined
    end.

%% strategy functions
init_target(_) ->
    {{0, none, none},
     fun ({LastAcc, AccUtility, _LastGen}) ->
             Offset = random:uniform(42) - 21,
             NewValue = LastAcc + Offset,
             {{LastAcc, AccUtility, NewValue}, NewValue}
     end,
     fun ({LastAcc, AccUtility, LastGen}, GenUtility) ->
             case AccUtility =:= none orelse
                 GenUtility > AccUtility of
                 true -> {LastGen, GenUtility, LastGen};
                 false -> {LastAcc, AccUtility, LastGen}
             end
     end}.

get_shrinker(_) ->
    proper_types:integer().

%% global fitness function
update_global_fitness(Utility) ->
    GlobalState = get(target_hillclimb_data),
    NewGlobalState = dict:fold(fun (T,{S,NF,UF}, AccIn) ->
                                       dict:store(T, {UF(S, Utility), NF, UF}, AccIn)
                               end,
                               dict:new(), GlobalState),
    put(target_hillclimb_data, NewGlobalState).
