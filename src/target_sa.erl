-module(target_sa).
-behaviour(target_strategy).
-export([init_strategy/1,
	 init_target/1,
	 store_target/2,
	 update_target_state/2,
	 retrieve_target/1,
	 update_global_fitness/1,
	 %% lib
	 integer/0,
	 integer/2,
	 float/0,
	 float/2]).

%% SA search strategy
-type temperature() :: float().

-record(sa_target, {first = null,
		    next = null,
		    current_generated = null,
		    last_generated = null
		   }).
-type sa_target() :: #sa_target{}.

-record(sa_data, {state = dict:new()             :: dict:dict(target_strategy:hash(), sa_target()),
		  k_max = 0                      :: integer(),
		  k_current = 0                  :: integer(),
		  p = fun (_, _, _) -> false end :: fun((float(), float(), temperature()) -> boolean()),
		  last_energy = null             :: float() | null
		 }).

-define(DEFAULT_STEPS, 1000).
-define(MAX_SIZE, 10000).

acceptance_function(EnergyCurrent, EnergyNew, _Temperature) ->
    %% Todo: exchange hill climbing with SA acceptance function
    EnergyNew > EnergyCurrent.

get_amount_of_steps({numtests, N, _}) ->
    N;
get_amount_of_steps({fails, Prop}) ->
    get_amount_of_steps(Prop);
get_amount_of_steps({on_output, _, Prop}) ->
    get_amount_of_steps(Prop);
get_amount_of_steps(_) ->
    N = get(target_sa_steps),
    case is_integer(N) of
	true -> N;
	_ -> ?DEFAULT_STEPS
    end.

init_strategy(Prop) ->
    put(target_sa_data, #sa_data{k_max = get_amount_of_steps(Prop),
				 p = fun acceptance_function/3
				}),
    Prop.

init_target([]) ->
    init_target(?MODULE:integer());
init_target(Opts) ->
    create_target(parse_opts(Opts)).

create_target(TargetState) ->
    {ok, InitialValue} = proper_gen:pick((TargetState#sa_target.first)),
    {TargetState#sa_target{last_generated = InitialValue },
     fun next_func/1,
     %% dummy local fitness function
     fun (S, _) -> S end}.

%% generating next element and updating the target state
next_func(State) ->
    %% retrieving temperature
    GlobalData = get(target_sa_data),
    Temperature = GlobalData#sa_data.k_current / GlobalData#sa_data.k_max,
    %% calculating the max generated size
    MaxSize = trunc(?MAX_SIZE * (1 - Temperature)) + 1,
    %% getting the generator for the next element (dependend on size and the last generated element)
    NextGenerator = (State#sa_target.next)(State#sa_target.last_generated, Temperature),
    %% generate the next element
    {ok, Generated} = proper_gen:pick(NextGenerator, MaxSize),
    %% return according to interface
    {State#sa_target{current_generated = Generated}, Generated}.

parse_opts(Opts) ->
    parse_opts(Opts, #sa_target{}).

parse_opts([], Opts) -> Opts;
parse_opts([Opt|T], Opts) ->
    parse_opts(T, parse_opt(Opt, Opts)).

parse_opt(Opt, Opts) ->
    case Opt of
	{first, G} -> Opts#sa_target{first = G};
	{next, G} ->  Opts#sa_target{next = G}
    end.

store_target(Key, Target) ->
    Data = get(target_sa_data),
    NewData = Data#sa_data{state = dict:store(Key, Target, (Data#sa_data.state))},
    put(target_sa_data, NewData),
    ok.

update_target_state(Key, TargetState) ->
    {_, N, F} = retrieve_target(Key),
    store_target(Key, {TargetState, N, F}).

retrieve_target(Key) ->
    Dict = (get(target_sa_data))#sa_data.state,
    case dict:is_key(Key, Dict) of
	true ->
	    dict:fetch(Key, Dict);
	false ->
	    undefined
    end.

update_global_fitness(Fitness) ->
    Data = get(target_sa_data),
    K_CURRENT = (Data#sa_data.k_current),
    K_MAX = (Data#sa_data.k_max),
    NextK = case K_CURRENT=:=K_MAX of
		true -> K_CURRENT;
		false -> K_CURRENT+1
	    end,    
    Temperature = K_CURRENT / K_MAX,
    NewData = case (Data#sa_data.last_energy =:= null) 
		  orelse
		  (Data#sa_data.p)(Data#sa_data.last_energy,
				   Fitness,
				   Temperature) of
		  true ->
		      %% accept new state
		      NewState = update_all_targets(Data#sa_data.state),
		      Data#sa_data{state = NewState, 
				   last_energy=Fitness, 
				   k_current = NextK};
		  false ->
		      %% reject new state
		      Data#sa_data{k_current = NextK}
	      end,
    put(target_sa_data, NewData),
    ok.

update_all_targets(TargetDict) ->
    %% update the last generated value with the current generated value (hence accepting new state)
    update_all_targets(TargetDict, dict:fetch_keys(TargetDict)).

update_all_targets(Dict,  []) ->
    Dict;
update_all_targets(Dict, [K|T]) ->
    FF = dict:fetch(K, Dict),
    {S, N, F} = FF,
    update_all_targets(dict:store(K, {S#sa_target{ last_generated = S#sa_target.current_generated }, N, F}, Dict),
		       T).

%% library
-include_lib("proper/include/proper_common.hrl").

integer() ->
    ?MODULE:integer(inf, inf).

integer(L, R) ->
    [{first, proper_types:integer(L, R)},
     {next, integer_next(L, R)}].

integer_next(L, R) ->
    fun (OldInstance, _Temperature) ->
	    ?LET(X, proper_types:integer(),
		 make_inrange(X + OldInstance, L, R))
    end.

float() ->
    ?MODULE:float(inf, inf).

float(L, R) ->
    [{first, proper_types:float(L, R)},
     {next, float_next(L, R)}].

float_next(L, R) ->
    fun (OldInstance, _Temperature) ->
	    ?LET(X, proper_types:float(),
		 make_inrange(X+OldInstance, L, R))
    end.

make_inrange(Val, L, R) when (R=:=inf orelse Val =< R) andalso (L=:=inf orelse Val >= L) -> Val;
make_inrange(Val, L, _R) when Val < L -> L;
make_inrange(Val, _L, R) when Val > R -> R.
