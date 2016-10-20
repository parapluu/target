%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(target_strategy).
-export([use_strategy/2, get_target/2, update_target/2, update_global/1, shrink_gen/1]).

-export_type([target_state/0, next_func/0, fitness_func/0,
	      target/0, options/0, property/0, generator/0]).

-type target_state() :: term().
-type next_func()    :: fun ((target_state()) -> {target_state(), any()}).
-type fitness_func() :: fun ((target_state(), target:fitness()) -> target_state()).

-type target()    :: {target_state(), next_func(), fitness_func()}.
-type options()   :: [{atom(), term()}].
-type property()  :: proper:outer_test().
-type strategy()  :: module().
-type generator() :: proper_types:raw_type().

%% behaviour for strategies
%% strategy global initializer
-callback init_strategy(property()) -> property().
%% target (one variable) initializer
-callback init_target(target:tmap()) -> target().
%% %% generator for shrinking
-callback get_shrinker(target:tmap()) -> generator().
%% store, and retrieve state
-callback store_target(target:key(), target_state()) -> 'ok'.
-callback retrieve_target(target:key()) -> target() | 'undefined'.
%% global update
-callback update_global_fitness(target:fitness()) -> 'ok'.

%% access to the current strategy
-define(STRATEGY, get(target_strategy)).

%% store the used strategy into the process dictionary
-spec use_strategy(strategy(), any()) -> property().
use_strategy(Strategy, Prop) ->
  put(target_strategy, Strategy),
  Strategy:init_strategy(Prop).

-spec get_target(target:key(), options()) -> target().
get_target(Key, Opts) ->
  Strategy = ?STRATEGY,
  case Strategy:retrieve_target(Key) of
    undefined ->
      FreshTarget = Strategy:init_target(Opts),
      Strategy:store_target(Key, FreshTarget),
      FreshTarget;
    StoredTarget ->
      StoredTarget
  end.

-spec update_target(target:key(), target_state()) -> 'ok'.
update_target(Key, State) ->
  {_, N, F} = (?STRATEGY):retrieve_target(Key),
  (?STRATEGY):store_target(Key, {State, N, F}).

-spec update_global(target:fitness()) -> 'ok'.
update_global(Fitness) ->
  (?STRATEGY):update_global_fitness(Fitness).

-spec shrink_gen(options()) -> generator().
shrink_gen(Opts) ->
  (?STRATEGY):get_shrinker(Opts).
