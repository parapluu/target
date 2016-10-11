%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(target_sa).
-behaviour(target_strategy).
-export([init_strategy/1,
         init_target/1,
         store_target/2,
         retrieve_target/1,
         update_global_fitness/1,
         get_shrinker/1,
         %% lib
         integer/0,
         integer/2,
         float/0,
         float/2]).

-include_lib("proper/include/proper_common.hrl").

%% SA search strategy
-type temperature() :: float().

-record(sa_target, {first = null,
                    next = null,
                    current_generated = null,
                    last_generated = null
                   }).
-type sa_target() :: #sa_target{}.

-record(sa_data, {state = dict:new()             :: dict:dict(target:key(), sa_target()),
                  %% max runs
                  k_max = 0                      :: integer(),
                  %% run number
                  k_current = 0                  :: integer(),
                  %% acceptance probability
                  p = fun (_, _, _) -> false end :: fun((float(), float(), temperature()) -> boolean()),
                  %% energy level
                  last_energy = null             :: float() | null,
                  %% temperature function
                  temperature = 1.0 :: float(),
                  temp_func = fun(_, _, _, _, _) -> 1.0 end :: fun(( %% old temperature
                                                                     float(),
                                                                     %% old energy level
                                                                     float(),
                                                                     %% new energy level
                                                                     float(),
                                                                     %% k_current
                                                                     integer(),
                                                                     %% k_max
                                                                     integer(),
                                                                     %% accepted or not
                                                                     boolean()) -> {float(), integer()})
                 }).

-define(DEFAULT_STEPS, 1000).
-define(MAX_SIZE, 10000).
-define(REHEAT_THRESHOLD, 5).

-define(RANDOM_PROPABILITY, (rand:uniform())).

print_accepted(State, Utility, Temperature) ->
  case get(target_print_accepted) of
    Printer when is_function(Printer) -> Printer(State, Utility);
    true -> io:format("Accepted at Fitness ~p and Temperature ~p ~n", [Utility, Temperature]);
    _ -> ok
  end.

acceptance_function_standard(EnergyCurrent, EnergyNew, Temperature) ->
  case EnergyNew > EnergyCurrent of
    true ->
      %% allways accept better results
      true;
    false ->
      %% probabilistic accepptance (allways between 0 and 0.5)
      AcceptancePropability  = try
                                 %%  1 / (1 + math:exp(abs(EnergyCurrent - EnergyNew) / Temperature))
                                 math:exp(-(EnergyCurrent - EnergyNew) / Temperature)
                               catch
                                 error:badarith -> 0.0
                               end,
      %% if random probability is less, accept
      ?RANDOM_PROPABILITY < AcceptancePropability
  end.

acceptance_function_normalized(EnergyCurrent, EnergyNew, Temperature) ->
  case EnergyNew > EnergyCurrent of
    true ->
      %% allways accept better results
      true;
    false ->
      %% probabilistic accepptance (allways between 0 and 0.5)
      AcceptancePropability  = try
                                 1 / (1 + math:exp( (1 -  (EnergyNew/EnergyCurrent)) / Temperature))
                               catch
                                 error:badarith -> 0.0
                               end,
      %% if random probability is less, accept
      ?RANDOM_PROPABILITY < AcceptancePropability
  end.

acceptance_function_hillclimbing(EnergyCurrent, EnergyNew, _Temperature) ->
  %% Hill-Climbing
  EnergyNew > EnergyCurrent.

temperature_function_fast_sa(_OldTemperature,
                             _OldEnergyLevel,
                             _NewEnergyLevel,
                             _K_Max,
                             K_Current,
                             Accepted) ->
  AdjustedK = case not Accepted of
                true ->
                  case get(target_se_reheat_counter) of
                    undefined ->
                      put(target_se_reheat_counter, 1),
                      K_Current + 1;
                    N when N >= ?REHEAT_THRESHOLD->
                      put(target_se_reheat_counter, 0),
                      max(1, K_Current - trunc(1.4 * ?REHEAT_THRESHOLD));
                    N ->
                      put(target_se_reheat_counter, N + 1),
                      K_Current + 1
                  end;
                false -> K_Current + 1
              end,
  {1 / max((AdjustedK / 4.0), 1.0), AdjustedK}.

temperature_function_fast2_sa(_OldTemperature,
                              _OldEnergyLevel,
                              _NewEnergyLevel,
                              K_Max,
                              K_Current,
                              _Accepted) ->
  %% AdjustedK = case not Accepted of
  %%                 true -> max(1, trunc(K_Current / 1.2));
  %%                 false -> K_Current + 1
  %%             end,
  {1.0 - math:sqrt(K_Current / K_Max), K_Current + 1}.

temperature_function_reheat_sa(OldTemperature,
                               OldEnergyLevel,
                               NewEnergyLevel,
                               K_Max,
                               K_Current,
                               Accepted) when is_integer(K_Current)->
  temperature_function_reheat_sa(OldTemperature,
                                 OldEnergyLevel,
                                 NewEnergyLevel,
                                 K_Max,
                                 {K_Current, K_Current},
                                 Accepted);
temperature_function_reheat_sa(_OldTemperature,
                               _OldEnergyLevel,
                               _NewEnergyLevel,
                               K_Max,
                               {K_Current, K_Counter},
                               Accepted) ->
  Scaling = 1.0 - (K_Counter / K_Max),
  AdjustedK = case not Accepted of
                true ->
                  case get(target_se_reheat_counter) of
                    undefined ->
                      put(target_se_reheat_counter, 1),
                      K_Current + 1;
                    N when N >= ?REHEAT_THRESHOLD->
                      put(target_se_reheat_counter, 0),
                      max(1, K_Current - trunc(Scaling * 5 * ?REHEAT_THRESHOLD));
                    N ->
                      put(target_se_reheat_counter, N + 1),
                      K_Current + 1
                  end;
                false -> K_Current + 1
              end,
  {1 / max((AdjustedK / 4.0), 1.0), {AdjustedK, K_Counter + 1}}.

temperature_function_standard_sa(_OldTemperature,
                                 _OldEnergyLevel,
                                 _NewEnergyLevel,
                                 K_Max,
                                 K_Current,
                                 _Accepted) ->
  {1.0 - (K_Current / K_Max), K_Current + 1}.

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

get_temperature_function() ->
  io:format("Temperature Function: \t"),
  case get(target_sa_tempfunc) of
    default ->
      io:format("default~n"),
      fun temperature_function_standard_sa/6;
    fast ->
      io:format("fast~n"),
      fun temperature_function_fast_sa/6;
    very_fast ->
      io:format("very fast~n"),
      fun temperature_function_fast2_sa/6;
    reheat ->
      io:format("decreasing reheating~n"),
      fun temperature_function_reheat_sa/6;
    Fun when is_function(Fun) ->
      case proplists:lookup(arity, erlang:fun_info(Fun)) of
        {arity, 6} ->
          io:format("configured ~p~n", [Fun]),
          Fun;
        _ ->
          io:format("wrong arity of configured temperature function; using default instead~n"),
          fun temperature_function_standard_sa/6
      end;
    undefined ->
      io:format("default~n"),
      fun temperature_function_standard_sa/6;
    _ ->
      io:format("undefined configured temperature function; using default instead~n"),
      fun temperature_function_standard_sa/6
  end.

get_acceptance_function() ->
  io:format("Acceptance Function: \t"),
  case get(target_sa_acceptfunc) of
    default ->
      io:format("default~n"),
      fun acceptance_function_standard/3;
    hillclimbing ->
      io:format("hillclimbing~n"),
      fun acceptance_function_hillclimbing/3;
    normalized ->
      io:format("normalized~n"),
      fun acceptance_function_normalized/3;
    Fun when is_function(Fun) ->
      case proplists:lookup(arity, erlang:fun_info(Fun)) of
        {arity, 3} ->
          io:format("configured ~p~n", [Fun]),
          Fun;
        _ ->
          io:format("wrong arity of configured acceptance function; using default instead~n"),
          fun acceptance_function_standard/3
      end;
    undefined ->
      io:format("default~n"),
      fun acceptance_function_standard/3;
    _ ->
      io:format("undefined configured acceptance function; using default instead~n"),
      fun acceptance_function_standard/3
  end.

-spec init_strategy(proper:outer_test()) -> proper:outer_test().
init_strategy(Prop) ->
  io:format("-- Simulated Anneahling Search Strategy --~n"),
  put(target_sa_data, #sa_data{k_max = get_amount_of_steps(Prop),
                               p = get_acceptance_function(),
                               temp_func = get_temperature_function()
                              }),
  Prop.

-spec init_target(target_strategy:options()) -> target_strategy:target().
init_target([]) ->
  init_target(?MODULE:integer());
init_target(Opts) ->
  create_target(parse_opts(Opts)).

create_target(TargetState) ->
  {ok, InitialValue} = proper_gen:clean_instance(proper_gen:safe_generate(TargetState#sa_target.first)),
  {TargetState#sa_target{last_generated = InitialValue },
   fun next_func/1,
   %% dummy local fitness function
   fun (S, _) -> S end}.

%% generating next element and updating the target state
next_func(State) ->
  %% retrieving temperature
  GlobalData = get(target_sa_data),
  Temperature = GlobalData#sa_data.temperature,
  %% calculating the max generated size
  %% MaxSize = trunc(?MAX_SIZE * Temperature) + 1,
  %% io:format("MaxSize: ~p Temperature: ~p ~n", [MaxSize, Temperature]),
  %% getting the generator for the next element (dependend on size and the last generated element)
  %% io:format("~p  -- ~p ~n", [State#sa_target.last_generated, State#sa_target.next]),
  NextGenerator = (State#sa_target.next)(State#sa_target.last_generated, Temperature),
  %% generate the next element
  {ok, Generated} = proper_gen:clean_instance(proper_gen:safe_generate(NextGenerator)),
  %% return according to interface
  {State#sa_target{current_generated = Generated}, Generated}.

parse_opts(Opts) ->
  case proper_types:is_type(Opts) of
    true ->
      %% automatically generate neighborhood function
      target_sa_gen:from_proper_generator(Opts);
    false ->
      parse_opts(Opts, #sa_target{})
  end.

parse_opts([], Opts) -> Opts;
parse_opts([Opt|T], Opts) ->
  parse_opts(T, parse_opt(Opt, Opts)).

parse_opt(Opt, Opts) ->
  case Opt of
    {first, G} -> Opts#sa_target{first = G};
    {next, G} ->  Opts#sa_target{next = G}
  end.

-spec store_target(target:key(), target_strategy:target()) -> 'ok'.
store_target(Key, Target) ->
  Data = get(target_sa_data),
  NewData = Data#sa_data{state = dict:store(Key, Target, (Data#sa_data.state))},
  put(target_sa_data, NewData),
  ok.

-spec retrieve_target(target:key()) -> target_strategy:target().
retrieve_target(Key) ->
  Dict = (get(target_sa_data))#sa_data.state,
  case dict:is_key(Key, Dict) of
    true ->
      dict:fetch(Key, Dict);
    false ->
      undefined
  end.

-spec update_global_fitness(target:fitness()) -> 'ok'.
update_global_fitness(Fitness) ->
  Data = get(target_sa_data),
  K_CURRENT = (Data#sa_data.k_current),
  K_MAX = (Data#sa_data.k_max),
  %% Temperature = 1 - (K_CURRENT / K_MAX),
  Temperature = Data#sa_data.temperature,
  NewData = case (Data#sa_data.last_energy =:= null)
              orelse
              (Data#sa_data.p)(Data#sa_data.last_energy,
                               Fitness,
                               Temperature) of
              true ->
                %% accept new state
                print_accepted(Data, Fitness, Temperature),
                NewState = update_all_targets(Data#sa_data.state),
                %% calculate new temperature
                {NewTemperature, AdjustedK} = (Data#sa_data.temp_func)(Temperature,
                                                                       Data#sa_data.last_energy,
                                                                       Fitness,
                                                                       K_MAX,
                                                                       K_CURRENT,
                                                                       true),
                Data#sa_data{state = NewState,
                             last_energy=Fitness,
                             k_current = AdjustedK,
                             temperature = NewTemperature};
              false ->
                %% reject new state
                %% calculate new temperature
                {NewTemperature, AdjustedK} = (Data#sa_data.temp_func)(Temperature,
                                                                       Data#sa_data.last_energy,
                                                                       Fitness,
                                                                       K_MAX,
                                                                       K_CURRENT,
                                                                       false),
                Data#sa_data{k_current = AdjustedK, temperature = NewTemperature}
            end,
  %% io:format("~p~n", [Data]),
  put(target_sa_data, NewData),
  %% timer:sleep(100),
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

-spec get_shrinker(target_strategy:options()) -> proper_types:type().
get_shrinker(Opts) ->
  ((parse_opts(Opts))#sa_target.first).

%% library
integer() ->
  ?MODULE:integer(inf, inf).

integer(L, R) ->
  [{first, proper_types:integer(L, R)},
   {next, integer_next(L, R)}].

integer_next(L, R) ->
  fun (OldInstance, Temperature) ->
      {LL, LR} = case L=:=inf orelse R=:=inf of
                   true ->
                     {inf, inf};
                   false ->
                     Limit = trunc(abs(L - R) * Temperature * 0.1) + 1,
                     {-Limit, Limit}
                 end,
      ?LET(X, proper_types:integer(LL, LR),
           make_inrange(OldInstance, X, L, R))
  end.

float() ->
  ?MODULE:float(inf, inf).

float(L, R) ->
  [{first, proper_types:float(L, R)},
   {next, float_next(L, R)}].

float_next(L, R) ->
  fun (OldInstance, Temperature) ->
      {LL, LR} = case L=:=inf orelse R=:=inf of
                   true ->
                     {inf, inf};
                   false ->
                     Limit = abs(L - R) * Temperature * 0.1,
                     {-Limit, Limit}
                 end,
      ?LET(X, proper_types:float(LL, LR),
           make_inrange(OldInstance, X, L, R))
  end.

make_inrange(Val, L, R) when (R=:=inf orelse Val =< R) andalso (L=:=inf orelse Val >= L) -> Val;
make_inrange(Val, L, _R) when Val < L -> L;
make_inrange(Val, _L, R) when Val > R -> R.

make_inrange(Val, Offset, L, R) when L=/=inf andalso Val + Offset < L -> make_inrange(Val - Offset, L, R);
make_inrange(Val, Offset, L, R) when R=/=inf andalso Val + Offset > R -> make_inrange(Val - Offset, L, R);
make_inrange(Val, Offset, L, R) -> make_inrange(Val + Offset, L, R).
