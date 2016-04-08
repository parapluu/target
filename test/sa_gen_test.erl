-module (sa_gen_test).

-export ([integer_test/0,
          list_test/0,
          combine_test/0,
          basic_test/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("target/include/target.hrl").
-inlcude_lib("eunit/include/eunit.hrl").

integer_test() ->
    proper_arith:rand_reseed(),
    {next, TG} = proplists:lookup(
                   next,
                   target_sa_gen:from_proper_generator(proper_types:integer())),
    appl(TG, 0, 100).

list_test() ->
    proper_arith:rand_reseed(),
    {next, TG} = proplists:lookup(
                   next,
                   target_sa_gen:from_proper_generator(proper_types:list(atom))),
    appl(TG, [], 100).

combine_test() ->
    proper_arith:rand_reseed(),
    {next, TG} = proplists:lookup(
                   next,
                   target_sa_gen:from_proper_generator(proper_types:list(proper_types:list(proper_types:integer())))),
    appl(TG, [], 100).

appl(_, A, 0) -> A;
appl(TG, A, X) -> appl(TG, TG(A, 0.5), X - 1).

prop_big_list() ->
    ?FORALL_SA(List, ?TARGET(target_sa_gen:from_proper_generator(proper_types:list(atom))),
               begin
                   L = length(List),
                   io:format("Length: ~p~n", [L]),
                   ?MAXIMIZE(-abs(L - 50)),
                   abs(L-50) > 2
               end).

basic_test() ->
    false = proper:quickcheck(prop_big_list(), [{to_file, user}, {numtests, 1000}]),
    [L] = proper:counterexample(),
    48 = length(L).
