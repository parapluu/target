-module(target_strategy).
-export([use_strategy/2, get_target/2, update_target/2, update_global/1, shrink_gen/1]).

-type target_state() :: any().
-type next_func() :: fun ((target_state()) -> {target_state(), any()}).
-type fitness_func() :: fun ((target_state(), target:fitness()) -> target_state()).

-type target() :: {target_state(), next_func(), fitness_func()}.

-type options() :: [{atom(), any()}].

-type property() :: proper:outer_test().
-type generator() :: proper_types:raw_type().

-export_type([target_state/0, next_func/0, fitness_func/0, target/0, options/0]).

%% behaviour for strategies
%% strategy global initializer
-callback init_strategy(property()) -> property().
%% target (one variable) initializer
-callback init_target(options()) -> target().
%% %% generator for shrinking
-callback get_shrinker(options()) -> generator().
%% store, update and retrieve state
-callback store_target(target:key(), target_state()) -> 'ok'.
-callback update_target_state(target:key(), target_state()) -> 'ok'.
-callback retrieve_target(target:key()) -> target() | undefined.
%% global update
-callback update_global_fitness(target:fitness()) -> 'ok'.

%% access to the current strategy
-define(STRATEGY, get(target_strategy)).

%% store the used strategy into the oprocess dictionary
-spec use_strategy(atom(), any()) -> property().
use_strategy(Strat, Prop) ->
    put(target_strategy, Strat),
    Strat:init_strategy(Prop).

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

-spec update_target(target:key(), target_state()) -> ok.
update_target(Key, State) ->
    (?STRATEGY):update_target_state(Key, State).

-spec update_global(target:fitness()) -> ok.
update_global(Fitness) ->
    (?STRATEGY):update_global_fitness(Fitness).

-spec shrink_gen(options()) -> generator().
shrink_gen(Opts) ->
    (?STRATEGY):get_shrinker(Opts).
