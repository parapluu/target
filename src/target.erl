-module(target).
-export([targeted/2, 
	 adjust/3]).

%% maximum phash value
-define(MAX_PHASH, 4294967296).

-include_lib("target.hrl").

targeted(Gen, Opts) ->
    proper_types:exactly(proper_types:lazy(fun() -> targeted_gen(Gen, Opts) end)).

%% @private
targeted_gen(Gen, Opts) ->
    Hash = erlang:phash(Gen, ?MAX_PHASH),
    {State, NextFunc, _FitnessFunc} = ?TARGET_STORRAGE_GET(Hash, Opts),
    {NewState, NextValue} = NextFunc(State),
    ?TARGET_STORRAGE_SET(Hash, NewState),
    NextValue.

adjust(Gen, Fitness, Threshold) ->
    set_fitness(Gen, Fitness),
    case Threshold of
	inf ->
	    true;
	_ ->
	    Fitness < Threshold
    end.

%% @private
set_fitness(Gen, Fitness) ->
    Hash = erlang:phash(Gen, ?MAX_PHASH),
    {State, _NextFunc, FitnessFunc} = ?TARGET_STORRAGE_GET(Hash, []),
    NewState = FitnessFunc(State, Fitness),
    ?TARGET_STORRAGE_SET(Hash, NewState).
