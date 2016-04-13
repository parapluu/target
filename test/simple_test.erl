-module(simple_test).
-export([enumerate_test/0,
	 sa_test/0,
	 shrinking_test/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("target/include/target.hrl").
-inlcude_lib("eunit/include/eunit.hrl").

enumerate_test() ->
    proper:quickcheck(prop_enum(),  [{to_file, user}, {numtests, 1000}]).

prop_enum() ->
    ?TARGET_STRATEGY(target_enumerate,
		    ?FORALL(I, ?NAMED_TARGET(X, integer(0,X)),
		     	     begin
		     		 io:format("I: ~p~n", [I]),
		     		 ?MAXIMIZE(I,X)
		     	     end)).

sa_test() ->
    put(target_sa_steps, 10000),
    proper:quickcheck(prop_sa(),  [{to_file, user}]).

prop_sa() ->
    ?TARGET_STRATEGY(target_sa,
		     ?FORALL({I,J}, {?TARGET(target_sa:integer()), ?TARGET(target_sa:integer())},
			     begin
				 io:format("I: ~p J: ~p~n", [I,J]),
				 ?MAXIMIZE(-(I+J))
			     end)).



check(I) ->
     I < 1000.

shrinking_test() ->
    false = proper:quickcheck(prop_shrinking(), [{to_file, user}, {numtests, 1000}]),
    [1000] = proper:counterexample().

prop_shrinking() ->
    ?TARGET_STRATEGY(target_sa,
		     proper:numtests(1000,
				     ?FORALL(I, ?TARGET(target_sa:integer()),
					     begin
						 ?MAXIMIZE(I),
						 check(I)
					     end))).


%% sized_test() ->
%%     proper:quickcheck(prop_sized(),  [{to_file, user}]).

%% sized_gen() ->
%%     ?SIZED(Size, sized_gen(Size)).

%% sized_gen(0) -> 0;
%% sized_gen(Size) -> oneof([0, 1+sized_gen(Size-1)]).

%% prop_sized() ->
%%     ?TARGET_STRATEGY(target_enumerate,
%% 		     ?FORALL({I, S}, ?TARGET(X, {exactly(X), sized_gen()}),
%% 		     	     begin
%% 		     		 io:format("I: ~p S: ~p~n", [I, S]),
%% 		     		 ?MAXIMIZE(I,X)
%% 		     	     end)).
