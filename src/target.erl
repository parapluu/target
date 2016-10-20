%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(target).

-export([targeted/3, adjust/2, adjust/3]).

-include_lib("proper/include/proper_common.hrl").  % uses ?LET and ?SHRINK

-export_type([key/0, fitness/0, tmap/0]).

-type key()     :: nonempty_string() | reference().
-type fitness() :: number().
-type tmap()    :: #{atom() => term()}.

-type generator() :: any().
-type threshold() :: fitness() | 'inf'.

-spec targeted(key(), generator(), tmap()) -> generator().
targeted(Key, Gen, TMap) ->
  ?SHRINK(proper_types:exactly(?LAZY(targeted_gen(Key, Gen, TMap))),
	  [target_strategy:shrink_gen(TMap)]).

%% @private
targeted_gen(Key, Gen, TMap) ->
  {State, NextFunc, _FitnessFunc} = target_strategy:get_target(Key, TMap),
  {NewState, NextValue} = NextFunc(State),
  target_strategy:update_target(Key, NewState),
  Gen(NextValue).

-spec adjust(fitness(), threshold()) -> boolean().
adjust(Fitness, Threshold) ->
  set_fitness(Fitness),
  check_threshold(Threshold, Fitness).

-spec adjust(fitness(), threshold(), key()) -> boolean().
adjust(Fitness, Threshold, Key) ->
  set_fitness(Fitness, Key),
  check_threshold(Threshold, Fitness).

%% @private
check_threshold(Threshold, Fitness) ->
  case Threshold of
    inf -> true;
    _ -> Fitness < Threshold
  end.

%% @private
set_fitness(Fitness, Key) ->
  {State, _NextFunc, FitnessFunc} = target_strategy:get_target(Key, []),
  NewState = FitnessFunc(State, Fitness),
  target_strategy:update_target(Key, NewState).

%% @private
set_fitness(Fitness) ->
  target_strategy:update_global(Fitness).
