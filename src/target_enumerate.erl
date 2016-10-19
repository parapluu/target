%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(target_enumerate).

-behaviour(target_strategy).

-export([init_strategy/1,
	 init_target/1,
	 store_target/2,
	 retrieve_target/1,
	 update_global_fitness/1,
	 get_shrinker/1
	]).

-spec init_strategy(Prop) -> Prop when Prop :: target_strategy:property().
init_strategy(Prop) ->
  erase(target_enumerate_data),
  undefined = put(target_enumerate_data, dict:new()),
  Prop.

-spec init_target(target_strategy:options()) -> target_strategy:target().
init_target(_) ->
  %% enumerate targets are all integer targets counting from 0 upwards
  {0, fun (Last) -> {Last+1, Last+1} end, fun (TargetState, _Fitness) -> TargetState end}.

-spec store_target(target:key(), target_strategy:target()) -> 'ok'.
store_target(Key, Target) ->
  NewDict = dict:store(Key, Target, get(target_enumerate_data)),
  put(target_enumerate_data, NewDict),
  ok.

-spec retrieve_target(target:key()) -> target_strategy:target() | 'undefined'.
retrieve_target(Key) ->
  Dict = get(target_enumerate_data),
  case dict:is_key(Key, Dict) of
    true ->
      dict:fetch(Key, Dict);
    false ->
      undefined
  end.

-spec update_global_fitness(target:fitness()) -> 'ok'.
update_global_fitness(_Fitness) ->
  ok.

-spec get_shrinker(target_strategy:options()) -> proper_types:type().
get_shrinker(_) ->
  proper_types:integer().
