%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module (sa_gen_test).

-export ([integer_test/0,
          list_test/0,
          combine_test/0,
          basic_test/0,
          basic2_test/0,
          let_test/0,
          graph_test/0,
          suchthat_test/0,
          union_test/0,
          weighted_union_test/0,
          lazy_test/0,
          sized_test/0,
          tuple_test/0,
          edge_test/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("target/include/target.hrl").
-inlcude_lib("eunit/include/eunit.hrl").

-define(PROPER_OPTIONS, [quiet, {numtests, 1000}]).

integer_test() ->
  put(target_sa_testing, true),
    proper:global_state_init_size(10),
    {next, TG} = proplists:lookup(
                   next,
                   target_sa_gen:from_proper_generator(proper_types:integer())),
    appl(TG, 0, 100).

list_test() ->
  put(target_sa_testing, true),
    proper:global_state_init_size(10),
    {next, TG} = proplists:lookup(
                   next,
                   target_sa_gen:from_proper_generator(proper_types:list(atom))),
    appl(TG, [], 100).

combine_test() ->
  put(target_sa_testing, true),
    proper:global_state_init_size(10),
    {next, TG} = proplists:lookup(
                   next,
                   target_sa_gen:from_proper_generator(proper_types:list(proper_types:list(proper_types:integer())))),
    appl(TG, [], 100).

appl(_, A, 0) -> A;
appl(TG, A, X) ->
    appl(TG, TG(A, 0.5), X - 1).


prop_big_list() ->
    ?FORALL_SA(List, ?TARGET(target_sa_gen:from_proper_generator(proper_types:list(atom))),
               begin
                   L = length(List),
                   ?MAXIMIZE(-abs(L - 50)),
                   abs(L-50) > 2
               end).

basic_test() ->
  put(target_sa_testing, true),
    false = proper:quickcheck(prop_big_list(), ?PROPER_OPTIONS),
    [L] = proper:counterexample(),
    48 = length(L).

prop_use_ngenerator() ->
  ?FORALL_SA(List, ?TARGET(proper_types:list(atom)),
             begin
                 L = length(List),
                 ?MAXIMIZE(-abs(L - 50)),
                 abs(L-50) > 2
             end).

basic2_test() ->
  put(target_sa_testing, true),
    false = proper:quickcheck(prop_use_ngenerator(), ?PROPER_OPTIONS),
    [L] = proper:counterexample(),
    48 = length(L).

even_int() ->
    ?LET(I, integer(), I*2).

prop_let() ->
    ?FORALL_SA(V, ?TARGET(even_int()),
               begin
                   ?MAXIMIZE(-V),
                   V rem 2 == 0
               end).

let_test() ->
  put(target_sa_testing, true),
    true = proper:quickcheck(prop_let(), ?PROPER_OPTIONS).

suchthat_gen() ->
    ?SUCHTHAT(I, integer(), I rem 2 =:= 0).

prop_suchthat() ->
    ?FORALL_SA(V, ?TARGET(suchthat_gen()),
               begin
                   io:format("~p~n", [V]),
                   ?MAXIMIZE(V),
                   V rem 2 =:= 0
               end).

suchthat_test() ->
  put(target_sa_testing, true),
    true = proper:quickcheck(prop_suchthat(), ?PROPER_OPTIONS).

prop_union() ->
    ?FORALL_SA(X, ?TARGET(proper_types:union([a,b,c])),
               lists:member(X, [a,b,c])).

union_test() ->
  put(target_sa_testing, true),
    true = proper:quickcheck(prop_union(), ?PROPER_OPTIONS).

prop_weighted_union() ->
    ?FORALL_SA(X, ?TARGET(proper_types:weighted_union([{1, a},{2, b},{3, c}])),
               lists:member(X, [a,b,c])).

weighted_union_test() ->
  put(target_sa_testing, true),
    true = proper:quickcheck(prop_weighted_union(), ?PROPER_OPTIONS).

tuple_type() ->
    proper_types:tuple([integer(), integer()]).

tuple_type_res() ->
    ?SUCHTHAT({V1, V2}, tuple_type(), V1>V2).

prop_tuple() ->
    ?FORALL_SA({L, R}, ?TARGET(tuple_type_res()),
               L>R).

tuple_test() ->
  put(target_sa_testing, true),
    true = proper:quickcheck(prop_tuple(), ?PROPER_OPTIONS).

prop_lazy() ->
  ?FORALL_SA(I, ?TARGET(?LAZY(?LET(I, integer(), I*2))), I rem 2 == 0).

lazy_test() ->
  put(target_sa_testing, true),
  true = proper:quickcheck(prop_lazy(), ?PROPER_OPTIONS).

sized_type() ->
  ?SIZED(S, lists:seq(0, S)).

prop_sized() ->
  ?FORALL_SA(L, sized_type(),
  begin
    ?MAXIMIZE(lists:sum(L)),
    length(L) < 42
  end).

sized_test() ->
  put(target_sa_testing, true),
  false = proper:quickcheck(prop_sized(), ?PROPER_OPTIONS),
  [C] = proper:counterexample(),
  length(C) =:= 42.

%% simple generator for a graph
simple_edge(V) ->
    ?SUCHTHAT({V1, V2}, {oneof(V), oneof(V)}, V1>V2).

prop_edge() ->
    ?FORALL_SA({L, R}, ?TARGET(target_sa_gen:from_proper_generator(simple_edge([1,2,3,4,5,6,7,8,9]))),
               L>R).

edge_test() ->
    true = proper:quickcheck(prop_edge(), ?PROPER_OPTIONS).

simple_edges(V) ->
    ?LET(Edges, list(simple_edge(V)),
         lists:usort(Edges)).

simple_graph() ->
    ?LET(RawV, non_empty(list(integer(1, inf))),
         begin
             V = lists:usort(RawV),
             case length(V)>1 of
                 true ->
                     ?LET(E, simple_edges(V),
                          {proper_types:shrink_list(V),
                           proper_types:shrink_list(E)});
                 _ ->
                     {proper_types:shrink_list(V),
                      []}
             end
         end).

prop_graph() ->
    ?FORALL_SA({V, E}, ?TARGET(target_sa_gen:from_proper_generator(simple_graph())),
               begin
                   ?MAXIMIZE((length(E) - length(V))),
                   %%    io:format("~p", [length(V) + length(E)]),
                   true
               end).

graph_test() ->
    put(target_sa_steps, 1000),
    put(target_sa_tempfunc, default),
    put(target_sa_acceptfunc, default),
    true = proper:quickcheck(prop_graph(), ?PROPER_OPTIONS).
