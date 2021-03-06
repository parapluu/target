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

-export([from_proper_generator/1, set_temperature_scaling/1, update_caches/1]).

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

-spec update_caches('accept' | 'reject') -> 'ok'.
update_caches(accept) ->
  put(target_sa_gen_cache_backup, get(target_sa_gen_cache)),
  ok;
update_caches(reject) ->
  put(target_sa_gen_cache, get(target_sa_gen_cache_backup)),
  ok.

-spec from_proper_generator(proper_types:type()) -> target:tmap().
from_proper_generator(RawGenerator) ->
  put(target_sa_gen_cache, #{}),
  put(target_sa_gen_cache_backup, #{}),
  #{first => RawGenerator, next => replace_generators(RawGenerator)}.

replace_generators(RawGen) ->
  Gen = proper_types:cook_outer(RawGen),
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

-spec set_temperature_scaling(boolean) -> 'ok'.
set_temperature_scaling(Enabled) ->
  put(target_sa_gen_temperature_scaling, Enabled).

temperature_scaling(Temp, Depth) ->
  case get(target_sa_gen_temperature_scaling) of
    false -> 1.0;
    full ->
      X = Temp * Depth,
      X / (1.0 + X);
    even ->
      Temp;
    _ ->
      X = Temp * Depth,
      0.1 * X / (1.0 + X)
  end.

calculate_temperature({Depth, Temp}) ->
  temperature_scaling(Temp, Depth);
calculate_temperature(Temp) ->
  temperature_scaling(Temp, 1).

%% sample
sample_from_type(Type, Temp) ->
  Gen = replace_generators(Type),
  {ok, Generated} = proper_gen:clean_instance(proper_gen:safe_generate(Type)),
  Gen(Generated, Temp).

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

make_inrange(Val, Offset, L, R) when L =/= inf andalso Val + Offset < L ->
  make_inrange(Val - Offset, L, R);
make_inrange(Val, Offset, L, R) when R =/= inf andalso Val + Offset > R ->
  make_inrange(Val - Offset, L, R);
make_inrange(Val, Offset, L, R) -> make_inrange(Val + Offset, L, R).

%% integers
is_integer_type(Type) ->
  has_same_generator(Type, proper_types:integer()).

integer_gen_sa({'$type', TypeProps}) ->
  {env, {Min, Max}} = proplists:lookup(env, TypeProps),
  fun (Base, TD) ->
      Temp = ?TEMP(TD),
      OffsetLimit = case Min =:= inf orelse Max =:= inf of
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
      OffsetLimit = case Min =:= inf orelse Max =:= inf of
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
  %% io:format("ChoiceE: ~p ~n", [Choice]),
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
  %% io:format("ChoiceL: ~p ~n", [Choice]),
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
      ElementType = replace_generators(InternalType),
      [ElementType(H, Temp) | list_gen_internal(T, Temp, InternalType, GrowthCoefficient)];
    nothing ->
      [H | list_gen_internal(T, Temp, InternalType, GrowthCoefficient)]
  end.

%% shrink_list
is_shrink_list_type(Type) ->
  has_same_generator(Type, proper_types:shrink_list([])).

shrink_list_gen_sa(Type) ->
  {ok, Env} = proper_types:find_prop(env, Type),
  replace_generators(Env).

%% vector
is_vector_type(Type) ->
  has_same_generator(Type, proper_types:vector(0, undef)).

vector_gen_sa(Type) ->
  {ok, InternalType} = proper_types:find_prop(internal_type, Type),
  ElementType = replace_generators(InternalType),
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
  ListGen = replace_generators(binary_list()),
  fun (Base, Temp) ->
      ListRepr = binary_to_list(Base),
      list_to_binary(ListGen(ListRepr, ?SLTEMP(Temp)))
  end.

binary_len_gen_sa(_Type) ->
  VectorGen = replace_generators(binary_vector()),
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
  ElementGens = lists:map(fun replace_generators/1,
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
  ElementGens = safe_map(fun (E) ->
                             {replace_generators(E), E}
                         end,
                         InternalTypes),
  fun ([], _) -> [];
      (Base, Temp) ->
      {NewFixedList, _} = safe_mapfoldl(
                            fun ({_, ElementType}, []) ->
                                {sample_from_type(ElementType, ?TEMP(Temp)), []};
                                ({ElementGen, ElementType}, [B|T]) ->
                                case proper_types:is_instance(B, ElementType) of
                                  true ->
                                    {ElementGen(B, ?TEMP(Temp)), T};
                                  false ->
                                    {sample_from_type(ElementType, ?TEMP(Temp)), [B|T]}
                                end;
                                ({ElementGen, ElementType}, ImproperTail) ->
                                case proper_types:is_instance(ImproperTail, ElementType) of
                                  true ->
                                    {ElementGen(ImproperTail, ?TEMP(Temp)), improper_ending};
                                  false ->
                                    {sample_from_type(ElementType, ?TEMP(Temp)), improper_ending}
                                end
                            end,
                            Base,
                            ElementGens),
      NewFixedList
  end.

%% union
%% weighted_union
is_union_type(Type) ->
  has_same_generator(Type, proper_types:union([42])) orelse
    has_same_generator(Type, proper_types:weighted_union([{1, 1}])).

union_gen_sa(Type) ->
  {ok, Env} = proper_types:find_prop(env, Type),
  %% io:format("union"),
  fun (Base, Temp) ->
      %% check if base is of any instance of the
      %% sub elements
      case lists:foldr(fun (E, Acc) ->
                           case proper_types:is_instance(Base, E) of
                             true -> [E|Acc];
                             false -> Acc
                           end
                       end, [], Env) of
        [] ->
          %% generate new
          Index = trunc(rand:uniform() * length(Env)) + 1,
          ET = lists:nth(Index, Env),
          {ok, Value} = proper_gen:clean_instance(proper_gen:safe_generate(ET)),
          Value;
        PossibleGens  ->
          C = rand:uniform(),
          C_Kep =         0.3 * ?TEMP(Temp),
          C_Chg = C_Kep + 0.3 * ?TEMP(Temp),
          if
            C < C_Kep ->
              %% keep
              Base;
            C < C_Chg ->
              %% change choice
              Index = trunc(rand:uniform() * length(Env)) + 1,
              ET = lists:nth(Index, Env),
              {ok, Value} = proper_gen:clean_instance(proper_gen:safe_generate(ET)),
              Value;
            true ->
              %% modify amongst the possible
              Index = trunc(rand:uniform() * length(PossibleGens)) + 1,
              ElementGen = lists:nth(Index, PossibleGens),
              SAGen = replace_generators(ElementGen),
              SAGen(Base, Temp)
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

set_cache_let(Type, Combined, Base) ->
  Key = erlang:phash2({let_type, Type, Combined}),
  M = get(target_sa_gen_cache),
  put(target_sa_gen_cache, M#{Key => Base}).

del_cache_let(Type, Combined) ->
  Key = erlang:phash2({let_type, Type, Combined}),
  M = get(target_sa_gen_cache),
  put(target_sa_gen_cache, maps:remove(Key, M)).

let_gen_sa(Type) ->
  {ok, Combine} = proper_types:find_prop(combine, Type),
  {ok, PartsType} = proper_types:find_prop(parts_type, Type),
  PartsGen = replace_generators(PartsType),
  fun (Base, Temp) ->
      LetOuter = case get_cached_let(Type, Base) of
                   {ok, Stored} ->
                     del_cache_let(Type, Base),
                     PartsGen(Stored, ?SLTEMP(Temp));
                   not_found ->
                     sample_from_type(PartsType, ?SLTEMP(Temp))
                 end,
      RawCombined = Combine(LetOuter),
      NewValue = match_cook(Base, RawCombined, Temp),
      set_cache_let(Type, NewValue, LetOuter),
      NewValue
  end.

match_cook(Base, Type = {'$type', _}, Temp) ->
  case Base of
    no_matching ->
      sample_from_type(Type, ?TEMP(Temp));
    _ ->
      Gen = replace_generators(Type),
      Gen(Base, ?SLTEMP(Temp))
  end;
match_cook(Base, RawType, Temp) ->
  if
    is_tuple(RawType) ->
      case is_set(RawType) orelse is_dict(RawType) of
        true ->
          %% we do not take appart erlangs dicts and sets
          %% io:format("Dict or Set~n"),
          RawType;
        _ ->
          MC = case is_tuple(Base) of
                 true ->
                   match_cook(tuple_to_list(Base), tuple_to_list(RawType), Temp);
                 false ->
                   match_cook(no_matching, tuple_to_list(RawType), Temp)
               end,
          list_to_tuple(MC)
      end;
    is_list(RawType) andalso is_list(Base) ->
      case safe_zip(Base, RawType) of
        {ok, ZippedBasesWithTypes} ->
          per_element_match_cook(ZippedBasesWithTypes, Temp);
        impossible ->
          sample_from_type(RawType, ?TEMP(Temp))
      end;
    is_list(RawType) ->
      %% the base is not matching
      per_element_match_cook(no_matching_list_zip(RawType), Temp);
    true ->
      RawType
  end.

%% handles improper lists
no_matching_list_zip([]) -> [];
no_matching_list_zip([H|T]) ->[{no_matching, H} | no_matching_list_zip(T)];
no_matching_list_zip(ImproperTail) -> {no_matching, ImproperTail}.

per_element_match_cook(ZippedBasesWithTypes, Temp) ->
  safe_map(fun ({B, RT}) -> match_cook(B, RT, Temp) end, ZippedBasesWithTypes).

safe_map(_Fun, []) -> [];
safe_map(Fun, [H|T]) ->
  [Fun(H) | safe_map(Fun, T)];
safe_map(Fun, ImpT) ->
  Fun(ImpT).

safe_mapfoldl(_, Acc, []) ->
  {[], Acc};
safe_mapfoldl(Fun, Acc, [H|T]) ->
  {NewElement, NewAcc} = Fun(H, Acc),
  {MapReturn, FoldReturn} = safe_mapfoldl(Fun, NewAcc, T),
  {[NewElement | MapReturn], FoldReturn};
safe_mapfoldl(Fun, Acc, ImproperTail) ->
  Fun(ImproperTail, Acc).

safe_zip(L, R) ->
  safe_zip(L, R, []).

safe_zip([], [], Acc) ->
  {ok, lists:reverse(Acc)};
safe_zip([HL | TL], [HR | TR], Acc) ->
  safe_zip(TL, TR, [{HL, HR} | Acc]);
safe_zip([], _, _) ->
  impossible;
safe_zip(_, [], _) ->
  impossible;
safe_zip(ITL, ITR, Acc) ->
  case is_list(ITL) orelse is_list(ITR) of
    true -> impossible;
    _ -> {ok, construct_improper(Acc, {ITL, ITR})}
  end.

-dialyzer({no_improper_lists, construct_improper/2}).

construct_improper([], IT) ->
  IT;
construct_improper([H|T], IT) ->
  [H | construct_improper(T, IT)].

%% unsafe checks
is_set({set, _, _, _, _, _, _, _, _}) -> true;
is_set(_) -> false.

is_dict({dict, _, _, _, _, _, _, _, _}) -> true;
is_dict(_) -> false.

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

save_sized_generation(Base, Temp, Next, First) ->
  try
    %% can fail with for example a fixed list
    Next(Base, Temp)
  catch
    error:function_clause ->
      {ok, E} = proper_gen:clean_instance(proper_gen:safe_generate(First)),
      E
  end.

wrapper_gen_sa(Type) ->
  case proper_types:get_prop(generator, Type) of
    {typed, Gen} ->
      if
        is_function(Gen, 1) ->
          fun (Base, Temp) ->
              Internal = replace_generators(Gen(Type)),
              Internal(Base, Temp)
          end;
        is_function(Gen, 2) ->
          fun (Base, Temp) ->
              Size = get_size(Type, ?TEMP(Temp)),
              Next = replace_generators(Gen(Type, Size)),
              save_sized_generation(Base, Temp, Next, Type)
          end
      end;
    Gen ->
      if
        is_function(Gen, 0) ->
          fun (Base, Temp) ->
              Internal = replace_generators(Gen()),
              Internal(Base, Temp)
          end;
        is_function(Gen, 1) ->
          fun (Base, Temp) ->
              Size = get_size(Type, ?TEMP(Temp)),
              Next = replace_generators(Gen(Size)),
              save_sized_generation(Base, Temp, Next, Type)
          end
      end
  end.

%% utility

dont_change(X) ->
  fun (_, _) -> X end.
