-module(target).
-export([targeted/3,
         adjust/2,
         adjust/3]).

-include_lib("proper/include/proper_common.hrl").

-type key() :: nonempty_string().
-type generator() :: any().
-type fitness() :: any().

-export_type([key/0, fitness/0]).

-spec targeted(key(), generator(), [{atom(), any()}]) -> generator().
targeted(Key,Gen, Opts) ->
    ?SHRINK(proper_types:exactly(?LAZY(targeted_gen(Key, Gen, Opts))),
            [target_strategy:shrink_gen(Opts)]).


%% @private
targeted_gen(Key, Gen, Opts) ->
    {State, NextFunc, _FitnessFunc} = target_strategy:get_target(Key, Opts),
    {NewState, NextValue} = NextFunc(State),
    target_strategy:update_target(Key, NewState),
    Gen(NextValue).

%% shrink_gen(ShrinkType, Gen) ->
%%     ?LET(X, ShrinkType,
%%          ?LAZY(Gen(X))).

adjust(Fitness, Threshold) ->
    set_fitness(Fitness),
    check_threshold(Threshold, Fitness).

adjust(Fitness, Threshold, Key) ->
    set_fitness(Fitness, Key),
    check_threshold(Threshold, Fitness).

%% @private
check_threshold(Threshold, Fitness) ->
    case Threshold of
        inf ->
            true;
        _ ->
            Fitness < Threshold
    end.

%% @private
set_fitness(Fitness, Key) ->
    {State, _NextFunc, FitnessFunc} = target_strategy:get_target(Key, []),
    NewState = FitnessFunc(State, Fitness),
    target_strategy:update_target(Key, NewState).

%% @private
set_fitness(Fitness) ->
    target_strategy:update_global(Fitness).
