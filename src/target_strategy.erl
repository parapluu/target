-module(target_strategy).
-export([use_strategy/2, get_target/2, update_target/2]).

-type target_state() :: any().
-type next_func() :: fun ((target_state()) -> {target_state(), any()}).
-type fitness_func() :: fun ((target_state()) -> target_state()).
-type hash() :: integer().
-type target() :: {target_state(), next_func(), fitness_func()}.

-type options() :: [{atom(), any()}].

-export_type([target_state/0, next_func/0, fitness_func/0, target/0, options/0]).

%% behaviour for strategies
%% strategy global initializer
-callback init_strategy() -> ok.
%% target (one variable) initializer
-callback init_target(options()) -> target().
%% store, update and retrieve state
-callback store_target(hash(), target_state()) -> 'ok'.
-callback update_target_state(hash(), target_state()) -> 'ok'.
-callback retrieve_target(hash()) -> target() | undefined.

%% access to the current strategy
-define(GET_STRATEGY, get(target_strategy)).

%% store the used strategy into the oprocess dictionary
-spec use_strategy(atom(), any()) -> ok.
use_strategy(Strat, Prop) ->
    put(target_strategy, Strat),
    ok = Strat:init_strategy(),
    Prop.

-spec get_target(hash(), options()) -> target().
get_target(Hash, Opts) ->
    Strategy = ?GET_STRATEGY,
    case Strategy:retrieve_target(Hash) of
	undefined ->
	    FreshTarget = Strategy:init_target(Opts),
	    Strategy:store_target(Hash, FreshTarget),
	    FreshTarget;
	StoredTarget ->
	    StoredTarget
    end.

-spec update_target(hash(), target_state()) -> ok.
update_target(Hash, State) ->
    Strategy = ?GET_STRATEGY,
    Strategy:update_target_state(Hash, State).
