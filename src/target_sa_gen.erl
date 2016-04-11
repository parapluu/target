-module (target_sa_gen).
-compile(export_all).

-define(STORRAGE, target_sa_gen_storrage).

-define(GENERATORS, [{fun is_atom/1, fun dont_change/1},
                     {fun is_list_type/1, fun list_gen_sa/1},
                     {fun is_integer_type/1, fun integer_gen_sa/1},
                     {fun is_float_type/1, fun float_gen_sa/1},
                     {fun is_atom_type/1, fun atom_gen_sa/1},
                     {fun is_vector_type/1, fun vector_gen_sa/1},
                     {fun is_binary_type/1, fun binary_gen_sa/1},
                     {fun is_binary_len_type/1, fun binary_len_gen_sa/1}]).

from_proper_generator(Generator) ->
    Hash = erlang:phash2(Generator),
    case get(?STORRAGE) of
        undefined ->
            put(?STORRAGE, #{ Hash => #{} });
        M ->
            put(?STORRAGE, M#{ Hash => #{} })
    end,
    [{first, Generator}, {next, replace_generators(Generator, Hash)}].

replace_generators(Gen, _Hash) ->
    case get_replacer(Gen) of
        {ok, Replacer} ->
            %% replaced generator
            Replacer(Gen);
        _ ->
            %% fallback
            io:format("Fallback using regular generator instead~n"),
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

has_same_generator({'$type', LTP}, {'$type', RTP}) ->
    LG = proplists:lookup(generator, LTP),
    RG = proplists:lookup(generator, RTP),
    LG=/=none andalso LG=:=RG;
has_same_generator(_, _) ->
    false.

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
    fun (Base, Temp) ->
            {OffsetMin, OffsetMax} = case Min=:=inf orelse Max=:=inf of
                                         true ->
                                             {trunc(-1000 * Temp), trunc(1000 * Temp)};
                                         false ->
                                             Limit = trunc(abs(Min - Max) * Temp * 0.1) + 1,
                                             {-Limit, Limit}
                                     end,
            Offset = proper_arith:rand_int(OffsetMin, OffsetMax),
            make_inrange(Base, Offset, Min, Max)
    end.

%% floats
is_float_type(Type) ->
    has_same_generator(Type, proper_types:float()).

float_gen_sa({'$type', TypeProps}) ->
    {env, {Min, Max}} = proplists:lookup(env, TypeProps),
    fun (Base, Temp) ->
            {OffsetMin, OffsetMax} = case Min=:=inf orelse Max=:=inf of
                                         true ->
                                             {-1000.0 * Temp, 1000.0 * Temp};
                                         false ->
                                             Limit = abs(Min - Max) * Temp * 0.1,
                                             {-Limit, Limit}
                                     end,
            Offset = proper_arith:rand_float(OffsetMin, OffsetMax),
            make_inrange(Base, Offset, Min, Max)
    end.

%% List
is_list_type(Type) ->
    has_same_generator(Type, proper_types:list(proper_types:atom())).

list_choice(empty, Temp) ->
    proper_arith:rand_reseed(),
    C = random:uniform(),
    C_Add = 0.5 * Temp,
    Choice = if
                 C < C_Add -> add;
                 true      -> nothing
             end,
    %% io:format("ChoiceE: ~p ~n", [C]),
    Choice;
list_choice(list, Temp) ->
    proper_arith:rand_reseed(),
    C = random:uniform(),
    C_Add =          0.2 * Temp,
    C_Del = C_Add + (0.1 * Temp),
    C_Mod = C_Del + (0.17 * Temp),
    Choice = if
                 C < C_Add -> add;
                 C < C_Del -> del;
                 C < C_Mod -> modify;
                 true      -> nothing
             end,
    %% io:format("ChoiceL: ~p ~n", [C]),
    Choice;
list_choice(vector, Temp) ->
    proper_arith:rand_reseed(),
    C = random:uniform(),
    C_Mod =          0.4 * Temp,
    Choice = if
                 C < C_Mod -> modify;
                 true      -> nothing
             end,
    %% io:format("ChoiceV: ~p ~n", [C]),
    Choice;
list_choice(tuple, Temp) ->
    list_choice(vector, Temp).

list_gen_sa(Type) ->
    InternalType = get_internal_type(Type),
    {next, ElementType} = proplists:lookup(next, from_proper_generator(InternalType)),
    fun GEN([], Temp) ->
            %% chance to add an element
            case list_choice(empty, Temp) of
                add ->
                    {ok, New} = proper_gen:pick(InternalType),
                    [New | GEN([], Temp)];
                nothing -> []
            end;
        GEN(L=[H|T], Temp) ->
            %% chance to modify current element
            %% chance to delete current element
            %% chance to add element infront of current element
            case list_choice(list, Temp) of
                add ->
                    {ok, New} = proper_gen:pick(InternalType),
                    [New | GEN(L, Temp)];
                del ->
                    GEN(T, Temp);
                modify ->
                    [ElementType(H, Temp) | GEN(T, Temp)];
                nothing ->
                    [H | GEN(T, Temp)]
            end
    end.

%% vector
is_vector_type(Type) ->
    has_same_generator(Type, proper_types:vector(0, undef)).

vector_gen_sa(Type) ->
    InternalType = get_internal_type(Type),
    {next, ElementType} = proplists:lookup(next, from_proper_generator(InternalType)),
    fun GEN([], _) ->
            [];
        GEN([H|T], Temp) ->
            case list_choice(vector, Temp) of
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
            list_to_binary(ListGen(ListRepr, Temp))
    end.

binary_len_gen_sa(_Type) ->
    {next, VectorGen} = proplists:lookup(next, from_proper_generator(binary_vector())),
    fun (Base, Temp) ->
            ListRepr = binary_to_list(Base),
            list_to_binary(VectorGen(ListRepr, Temp))
    end.

%% bitstrings

%% tuples
is_tuple_type(Type) ->
    has_same_generator(Type, proper_types:tuple([undef])).

tuple_gen_sa(Type) ->
    InternalTypes = get_internal_types(Type),
    ElementGens = lists:map(fun (E) ->
                                    {next, Gen} = proplists:lookup(next, from_proper_generator(E)),
                                    Gen end,
                            InternalTypes),
    fun (Base, Temp) ->
            ListRepr = tuple_to_list(Base),
            NewTupleAsList = lists:map(fun ({Gen, Elem}) ->
                                               case list_choice(tuple, Temp) of
                                                   nothing -> Elem;
                                                   modify -> Gen(Elem, Temp)
                                               end
                                       end,
                                       lists:zip(ElementGens, ListRepr)),
            list_to_tuple(NewTupleAsList)
    end.

%% union

%% weighted_union

%% utility functions

%% let

%% lazy

%% sized

%% shrink

%% letshrink

%% suchthat

%% suchthatmaybe



get_internal_type({'$type', TypeProperties}) ->
    case proplists:lookup(internal_type, TypeProperties) of
        {internal_type, Type} -> Type;
        _ -> no_internal_type
    end;
get_internal_type(_) ->
    not_a_type.

get_internal_types({'$type', TypeProperties}) ->
    case proplists:lookup(internal_types, TypeProperties) of
        {internal_types, Types} -> tuple_to_list(Types);
        _ -> no_internal_types
    end;
get_internal_types(_) ->
    not_a_type.

get_length({'$type', TypeProperties}) ->
    case proplists:lookup(env, TypeProperties) of
        {env, Length} -> case is_integer(Length) of
                             true -> Length;
                             _ -> no_length
                         end;
        _ -> no_length
    end;
get_length(_) ->
    not_a_type.

dont_change(X) ->
    fun (_, _) -> X end.
