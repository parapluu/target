%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(target_sa_gen).

-export([from_proper_generator/1]).

-define(STORAGE, target_sa_gen_storage).

-define(GENERATORS, [{fun is_atom/1, fun dont_change/1},
                     {fun is_list_type/1, fun list_gen_sa/1},
                     {fun is_fixed_list_type/1, fun fixed_list_gen_sa/1},
                     {fun is_integer_type/1, fun integer_gen_sa/1},
                     {fun is_float_type/1, fun float_gen_sa/1},
                     {fun is_atom_type/1, fun atom_gen_sa/1},
                     {fun is_vector_type/1, fun vector_gen_sa/1},
                     {fun is_tuple_type/1, fun tuple_gen_sa/1},
                     {fun is_binary_type/1, fun binary_gen_sa/1},
                     {fun is_binary_len_type/1, fun binary_len_gen_sa/1},
                     {fun is_let_type/1, fun let_gen_sa/1},
                     {fun is_shrink_list_type/1, fun shrink_list_gen_sa/1},
                     {fun is_union_type/1, fun union_gen_sa/1},
                     {fun is_wrapper_type/1, fun wrapper_gen_sa/1},
                     {fun is_exactly_type/1, fun exactly_gen_sa/1}]).

-define(TEMP(T), calculate_temperature(T)).
-define(SLTEMP(T), adjust_temperature(T)).

-spec from_proper_generator(proper_types:type()) -> proplists:proplist().
from_proper_generator(RawGenerator) ->
  ok = case get(target_sa_gen_cache) of
         undefined ->
           put(target_sa_gen_cache, #{}),
           ok;
         _ ->
           %% use existing cache
           ok
       end,
  Generator = cook(RawGenerator),
  Hash = erlang:phash2(Generator),
  case get(?STORAGE) of
    undefined ->
      put(?STORAGE, #{ Hash => #{} });
    M ->
      put(?STORAGE, M#{ Hash => #{} })
  end,
  [{first, Generator}, {next, replace_generators(Generator, Hash)}].

cook(Type = {'$type',_Props}) ->
  Type;
cook(RawType) ->
  if
    is_tuple(RawType) ->
      proper_types:tuple(tuple_to_list(RawType));
    %% CAUTION: this must handle improper lists
    is_list(RawType)  ->
      proper_types:fixed_list(RawType);
    %% default case (covers integers, floats, atoms, binaries, ...):
    true              ->
      proper_types:exactly(RawType)
  end.

replace_generators(Gen, _Hash) ->
  case get_replacer(Gen) of
    {ok, Replacer} ->
      %% replaced generator
      UnrestrictedGenerator = Replacer(Gen),
      RestrictedGenerator = apply_constraints(UnrestrictedGenerator, Gen),
      apply_temperature_scaling(RestrictedGenerator);
    _ ->
      %% fallback
      case proper_types:is_type(Gen) of
        true ->
          %% warning
          case get(target_sa_testing) of
            true -> error(target_sa_fallback);
            false -> io:format("Fallback using regular generator instead: ~p~n", [Gen])
          end;
        false ->
          %% literal value -> no warning
          ok
      end,
      fun (_, _) -> Gen end
  end.

get_replacer(Type) ->
  get_replacer(Type, ?GENERATORS).

get_replacer(_, []) ->
  {error, type_not_found};
get_replacer(Type, [ {Guard, Replacer} | Tail]) ->
  case Guard(Type) of
    true -> {ok, Replacer};
    _ -> get_replacer(Type, Tail)
  end.

has_same_generator(LT, RT) ->
  case proper_types:find_prop(generator, LT) of
    {ok, LG} ->
      {ok, RG} = proper_types:find_prop(generator, RT),
      LG =:= RG;
    error -> false
  end.

apply_constraints(UnrestrictedGenerator, Type) ->
  fun (Base, Temp) ->
      Tries = get('$constraint_tries'),
      restrict_generation(UnrestrictedGenerator, Base, Temp, Tries, Type, none)
  end.

restrict_generation(_, _, _, 0, _, none) -> throw(cannot_satisfy_constraint);
restrict_generation(_, _, _, 0, _, {ok, WeakInstance}) -> WeakInstance;
restrict_generation(Gen, B, T, TriesLeft, Type, WeakInstance) ->
  Instance = Gen(B, T),
  case proper_types:satisfies_all(Instance, Type) of
    {true, true} ->
      %% strong
      Instance;
    {true, _} ->
      %% weak
      restrict_generation(Gen, B, T, TriesLeft - 1, Type, {ok, Instance});
    _ ->
      %% not at all
      restrict_generation(Gen, B, T, TriesLeft - 1, Type, WeakInstance)
  end.

apply_temperature_scaling(Generator) ->
  fun (Base, Temp) ->
      %%     Generator(Base, Temp)
      case Temp of
        {Depth, Temperature} -> Generator(Base, {Depth + 1, Temperature});
        _ -> Generator(Base, {1, Temp})
      end
  end.

adjust_temperature({Depth, Temperature}) ->
  {Depth - 1, Temperature};
adjust_temperature(Temp) ->
  Temp.

temperature_scaling(X) ->
  X / (1.0 + X).

calculate_temperature({Depth, Temp}) ->
  temperature_scaling(Temp*Depth);
calculate_temperature(Temp) ->
  temperature_scaling(Temp).

%% exactly
is_exactly_type(Type) ->
  proper_types:get_prop(kind, Type) =:= basic andalso
    not proper_types:is_type(proper_types:get_prop(env, Type)).

exactly_gen_sa({'$type', TypeProps}) ->
  {env, Value} = proplists:lookup(env, TypeProps),
  fun (_, _) -> Value end.

%% Numbers

%% utility functions
make_inrange(Val, L, R) when (R=:=inf orelse Val =< R) andalso (L=:=inf orelse Val >= L) -> Val;
make_inrange(Val, L, _R) when Val < L -> L;
make_inrange(Val, _L, R) when Val > R -> R.

make_inrange(Val, Offset, L, R) when L=/=inf andalso Val + Offset < L -> make_inrange(Val - Offset, L, R);
make_inrange(Val, Offset, L, R) when R=/=inf andalso Val + Offset > R -> make_inrange(Val - Offset, L, R);
make_inrange(Val, Offset, L, R) -> make_inrange(Val + Offset, L, R).

%% integers
is_integer_type(Type) ->
  has_same_generator(Type, proper_types:integer()).

integer_gen_sa({'$type', TypeProps}) ->
  {env, {Min, Max}} = proplists:lookup(env, TypeProps),
  fun (Base, TD) ->
      Temp = ?TEMP(TD),
      OffsetLimit = case Min=:=inf orelse Max=:=inf of
                      true ->
                        trunc(1000 * Temp);
                      false ->
                        trunc(abs(Min - Max) * Temp * 0.1) + 1
                    end,
      Offset = proper_arith:rand_int(-OffsetLimit, OffsetLimit),
      make_inrange(Base, Offset, Min, Max)
  end.

%% floats
is_float_type(Type) ->
  has_same_generator(Type, proper_types:float()).

float_gen_sa({'$type', TypeProps}) ->
  {env, {Min, Max}} = proplists:lookup(env, TypeProps),
  fun (Base, _TD) ->
      %% Temp = ?TEMP(TD),
      OffsetLimit = case Min=:=inf orelse Max=:=inf of
                      true ->
                        10.0;
                      false ->
                        abs(Min - Max) * 0.001
                    end,
      Offset = proper_arith:rand_float(-OffsetLimit, OffsetLimit),
      make_inrange(Base, Offset, Min, Max)
  end.

%% List
is_list_type(Type) ->
  has_same_generator(Type, proper_types:list(proper_types:atom())).

list_choice(empty, Temp) ->
  C = rand:uniform(),
  C_Add = 0.5 * Temp,
  Choice = if
             C < C_Add -> add;
             true      -> nothing
           end,
  %% io:format("ChoiceE: ~p ~n", [C]),
  Choice;
list_choice({list, GrowthCoefficient}, Temp) ->
  C = rand:uniform(),
  AddCoefficient = 0.6 * GrowthCoefficient,
  DelCoefficient = 0.6 * (1- GrowthCoefficient),
  C_Add =          AddCoefficient * Temp,
  C_Del = C_Add + (DelCoefficient * Temp),
  C_Mod = C_Del + (0.3 * Temp),
  Choice = if
             C < C_Add -> add;
             C < C_Del -> del;
             C < C_Mod -> modify;
             true      -> nothing
           end,
  %% io:format("ChoiceL: ~p ~n", [C]),
  Choice;
list_choice(vector, Temp) ->
  C = rand:uniform(),
  C_Mod = 0.4 * Temp,
  Choice = if
             C < C_Mod -> modify;
             true      -> nothing
           end,
  %% io:format("ChoiceV: ~p ~n", [C]),
  Choice;
list_choice(tuple, Temp) ->
  list_choice(vector, Temp).

list_gen_sa(Type) ->
  {ok, InternalType} = proper_types:find_prop(internal_type, Type),
  %% io:format("list"),
  fun (Base, Temp) ->
      GrowthCoefficient = (rand:uniform() * 0.8) + 0.1,
      list_gen_internal(Base, Temp, InternalType, GrowthCoefficient)
  end.

list_gen_internal([], Temp, InternalType, GrowthCoefficient) ->
  %% chance to add an element
  case list_choice(empty, ?TEMP(Temp)) of
    add ->
      {ok, New} = proper_gen:clean_instance(proper_gen:safe_generate(InternalType)),
      [New | list_gen_internal([], Temp, InternalType, GrowthCoefficient)];
    nothing -> []
  end;
list_gen_internal(L=[H|T], Temp, InternalType, GrowthCoefficient) ->
  %% chance to modify current element
  %% chance to delete current element
  %% chance to add element infront of current element
  case list_choice({list, GrowthCoefficient}, ?TEMP(Temp)) of
    add ->
      {ok, New} = proper_gen:clean_instance(proper_gen:safe_generate(InternalType)),
      [New | list_gen_internal(L, Temp, InternalType, GrowthCoefficient)];
    del ->
      list_gen_internal(T, Temp, InternalType, GrowthCoefficient);
    modify ->
      {next, ElementType} = proplists:lookup(next, from_proper_generator(InternalType)),
      [ElementType(H, Temp) | list_gen_internal(T, Temp, InternalType, GrowthCoefficient)];
    nothing ->
      [H | list_gen_internal(T, Temp, InternalType, GrowthCoefficient)]
  end.

%% shrink_list
is_shrink_list_type(Type) ->
  has_same_generator(Type, proper_types:shrink_list([])).

shrink_list_gen_sa(Type) ->
  {ok, Env} = proper_types:find_prop(env, Type),
  {next, TypeGen} = proplists:lookup(next, from_proper_generator(Env)),
  TypeGen.

%% vector
is_vector_type(Type) ->
  has_same_generator(Type, proper_types:vector(0, undef)).

vector_gen_sa(Type) ->
  {ok, InternalType} = proper_types:find_prop(internal_type, Type),
  {next, ElementType} = proplists:lookup(next, from_proper_generator(InternalType)),
  fun GEN([], _) ->
      [];
      GEN([H|T], Temp) ->
      case list_choice(vector, ?TEMP(Temp)) of
        modify ->
          [ElementType(H, Temp) | GEN(T, Temp)];
        nothing ->
          [H | GEN(T, Temp)]
      end
  end.

%% atom
is_atom_type(Type) ->
  has_same_generator(Type, proper_types:atom()).

atom_gen_sa(_AtomType) ->
  StringType = proper_types:list(proper_types:integer(0, 255)),
  StringGen = list_gen_sa(StringType),
  fun (Base, Temp) ->
      StringRepr = atom_to_list(Base),
      list_to_atom(StringGen(StringRepr, Temp))
  end.

%% binary
is_binary_type(Type) ->
  has_same_generator(Type, proper_types:binary()).

is_binary_len_type(Type) ->
  has_same_generator(Type, proper_types:binary(1)).

binary_list() ->
  proper_types:list(proper_types:integer(0, 255)).

binary_vector() ->
  proper_types:vector(42, proper_types:integer(0, 255)).

binary_gen_sa(_Type) ->
  {next, ListGen} = proplists:lookup(next, from_proper_generator(binary_list())),
  fun (Base, Temp) ->
      ListRepr = binary_to_list(Base),
      list_to_binary(ListGen(ListRepr, ?SLTEMP(Temp)))
  end.

binary_len_gen_sa(_Type) ->
  {next, VectorGen} = proplists:lookup(next, from_proper_generator(binary_vector())),
  fun (Base, Temp) ->
      ListRepr = binary_to_list(Base),
      list_to_binary(VectorGen(ListRepr, ?SLTEMP(Temp)))
  end.

%% bitstrings

%% tuples
is_tuple_type(Type) ->
  has_same_generator(Type, proper_types:tuple([undef])).

tuple_gen_sa(Type) ->
  {ok, InternalTuple} = proper_types:find_prop(internal_types, Type),
  InternalTypes = tuple_to_list(InternalTuple),
  ElementGens = lists:map(fun (E) ->
                              {next, Gen} = proplists:lookup(next, from_proper_generator(E)),
                              Gen end,
                          InternalTypes),
  fun ({}, _) -> {};
      (Base, Temp) ->
      ListRepr = tuple_to_list(Base),
      NewTupleAsList = lists:map(fun ({Gen, Elem}) ->
                                     case list_choice(tuple, ?TEMP(Temp)) of
                                       nothing -> Elem;
                                       modify -> Gen(Elem, Temp)
                                     end
                                 end,
                                 lists:zip(ElementGens, ListRepr)),
      list_to_tuple(NewTupleAsList)
  end.

%% fixed list
is_fixed_list_type(Type) ->
  has_same_generator(Type, proper_types:fixed_list([])).

fixed_list_gen_sa(Type) ->
  {ok, InternalTypes} = proper_types:find_prop(internal_types, Type),
  ElementGens = lists:map(fun (E) ->
                              {next, Gen} = proplists:lookup(next, from_proper_generator(E)),
                              Gen
                          end,
                          InternalTypes),
  fun ([], _) -> [];
      (Base, Temp) ->
      lists:map(fun ({Gen, Elem}) ->
                    case list_choice(tuple, ?TEMP(Temp)) of
                      nothing -> Elem;
                      modify -> Gen(Elem, Temp)
                    end
                end,
                lists:zip(ElementGens, Base))
  end.

%% union
%% weighted_union
is_union_type(Type) ->
  has_same_generator(Type, proper_types:union([42])) orelse
    has_same_generator(Type, proper_types:weighted_union([{1, 1}])).

get_cached_union(Type, Combined) ->
  Key = erlang:phash2({union_type, Type, Combined}),
  case get(target_sa_gen_cache) of
    #{Key := Base} -> {ok, Base};
    _ -> not_found
  end.

set_cache_union(Type, Base, Combined) ->
  Key = erlang:phash2({union_type, Type, Combined}),
  M = get(target_sa_gen_cache),
  put(target_sa_gen_cache, M#{Key => Base}).

union_gen_sa(Type) ->
  {ok, Env} = proper_types:find_prop(env, Type),
  %% io:format("union"),
  fun (Base, Temp) ->
      C = rand:uniform(),
      C_Chg = 0.5 * ?TEMP(Temp),
      if
        C < C_Chg ->
          Index = trunc(rand:uniform() * length(Env)) + 1,
          ET = lists:nth(Index, Env),
          {ok, Value} = proper_gen:clean_instance(proper_gen:safe_generate(ET)),
          set_cache_union(Type, Value, ET),
          Value;
        true ->
          case get_cached_union(Type, Base) of
            {ok, ET} ->
              %% this type generated Base
              {next, ETGen} = proplists:lookup(next, from_proper_generator(ET)),
              Modified = ETGen(Base, Temp),
              set_cache_union(Type, Modified, ET),
              Modified;
            not_found ->
              %% first time
              Index = trunc(rand:uniform() * length(Env)) + 1,
              ET = lists:nth(Index, Env),
              {ok, Value} = proper_gen:clean_instance(proper_gen:safe_generate(ET)),
              set_cache_union(Type, Value, ET),
              Value
          end

      end
  end.

%% let
is_let_type({'$type', Props}) ->
  {kind, constructed} =:= proplists:lookup(kind, Props) andalso
    {shrink_to_parts, false} =:= proplists:lookup(shrink_to_parts, Props);
is_let_type(_) ->
  false.

get_cached_let(Type, Combined) ->
  Key = erlang:phash2({let_type, Type, Combined}),
  case get(target_sa_gen_cache) of
    #{Key := Base} -> {ok, Base};
    _ -> not_found
  end.

set_cache_let(Type, Base, Combined) ->
  Key = erlang:phash2({let_type, Type, Combined}),
  M = get(target_sa_gen_cache),
  put(target_sa_gen_cache, M#{Key => Base}).

let_gen_sa(Type) ->
  {ok, Combine} = proper_types:find_prop(combine, Type),
  {ok, PartsType} = proper_types:find_prop(parts_type, Type),
  {next, PartsGen} = proplists:lookup(next, from_proper_generator(PartsType)),
  fun (Base, Temp) ->
      LetOuter = case get_cached_let(Type, Base) of
                   {ok, Stored} ->
                     C = rand:uniform(),
                     C_Cnt = case ?TEMP(Temp) of
                               0.0 -> 0.0;
                               T -> math:sqrt(T)
                             end,
                     if
                       C < C_Cnt ->
                         PartsGen(Stored, ?SLTEMP(Temp));
                       true ->
                         Stored
                     end;
                   not_found ->
                     %% first time running the let
                     {ok, Generated} = proper_gen:clean_instance(proper_gen:safe_generate(PartsType)),
                     Generated
                 end,
      Combined = Combine(LetOuter),
      NewValue = case proper_types:is_type(Combined) of
                   true ->
                     {next, InternalGen} = proplists:lookup(next, from_proper_generator(Combined)),
                     InternalGen(Base, Temp);
                   false ->
                     Combined
                 end,
      set_cache_let(Type, LetOuter, NewValue),
      NewValue
  end.

%% lazy
%% sized
is_wrapper_type(Type) ->
  {ok, wrapper} =:= proper_types:find_prop(kind, Type).

get_cached_size(Type) ->
  Key = erlang:phash2({sized_type, Type}),
  case get(target_sa_gen_cache) of
    #{Key := Base} -> {ok, Base};
    _ -> not_found
  end.

set_cache_size(Type, Size) ->
  Key = erlang:phash2({sized_type, Type}),
  M = get(target_sa_gen_cache),
  put(target_sa_gen_cache, M#{Key => Size}).

get_size(Type, Temp) ->
  Size = case get_cached_size(Type) of
           not_found ->
             %% use random initial size
             %% proper:get_size(Type);
             trunc(rand:uniform() * 21 + 1);
           {ok, Base} ->
             %% alternate base size (max size is not accessible from the generator)
             OffsetLimit = trunc(21 * Temp + 1),
             Offset = trunc(rand:uniform() * OffsetLimit + 1),
             make_inrange(Base, Offset, 1, 42)
         end,
  set_cache_size(Type, Size),
  Size.

save_sized_generation(Base, Temp, SAGen) ->
  {next, Internal} = proplists:lookup(next, SAGen),
  try
    %% can fail with for example a fixed list
    Internal(Base, Temp)
  catch
    error:function_clause ->
      %% fixed list will fail here
      {first, InternalInit} = proplists:lookup(first, SAGen),
      {ok, E} = proper_gen:clean_instance(proper_gen:safe_generate(InternalInit)),
      E
  end.

wrapper_gen_sa(Type) ->
  case proper_types:get_prop(generator, Type) of
    {typed, Gen} ->
      if
        is_function(Gen, 1) ->
          fun (Base, Temp) ->
              {next, Internal} = proplists:lookup(next, from_proper_generator(Gen(Type))),
              Internal(Base, Temp)
          end;
        is_function(Gen, 2) ->
          fun (Base, Temp) ->
              Size = get_size(Type, ?TEMP(Temp)),
              SAGen = from_proper_generator(Gen(Type, Size)),
              save_sized_generation(Base, Temp, SAGen)
          end
      end;
    Gen ->
      if
        is_function(Gen, 0) ->
          fun (Base, Temp) ->
              {next, Internal} = proplists:lookup(next, from_proper_generator(Gen())),
              Internal(Base, Temp)
          end;
        is_function(Gen, 1) ->
          fun (Base, Temp) ->
              Size = get_size(Type, ?TEMP(Temp)),
              SAGen = from_proper_generator(Gen(Size)),
              save_sized_generation(Base, Temp, SAGen)
          end
      end
  end.

%% utility

dont_change(X) ->
  fun (_, _) -> X end.
