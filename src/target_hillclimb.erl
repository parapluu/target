%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

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
-spec init_strategy(proper:outer_test()) -> proper:outer_test().
init_strategy(Prop) ->
    erase(target_hillclimb_data),
    undefined = put(target_hillclimb_data, dict:new()),
    Prop.

%% storage functions
-spec store_target(target:key(), target_strategy:target()) -> 'ok'.
store_target(Key, Target) ->
    NewDict = dict:store(Key, Target, get(target_hillclimb_data)),
    put(target_hillclimb_data, NewDict),
    ok.

-spec retrieve_target(target:key()) -> target_strategy:target().
retrieve_target(Key) ->
    Dict = get(target_hillclimb_data),
    case dict:is_key(Key, Dict) of
        true ->
            dict:fetch(Key, Dict);
        false ->
            undefined
    end.

%% strategy functions
-spec init_target(target_strategy:options()) -> target_strategy:target().
init_target(_) ->
    {{0, none, none},
     fun ({LastAcc, AccUtility, _LastGen}) ->
             Offset = random:uniform()*20 - 10,
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

-spec get_shrinker(target_strategy:options()) -> proper_types:type().
get_shrinker(_) ->
    proper_types:integer().

%% global fitness function
-spec update_global_fitness(target:fitness()) -> 'ok'.
update_global_fitness(Utility) ->
    GlobalState = get(target_hillclimb_data),
    NewGlobalState = dict:fold(fun (T,{S,NF,UF}, AccIn) ->
                                       dict:store(T, {UF(S, Utility), NF, UF}, AccIn)
                               end,
                               dict:new(), GlobalState),
    put(target_hillclimb_data, NewGlobalState),
    ok.
