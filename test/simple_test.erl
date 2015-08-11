-module(simple_test).
-export([enumerate_test/0,
	 sa_test/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("target/include/target.hrl").
-inlcude_lib("eunit/include/eunit.hrl").

enumerate_test() ->
    proper:quickcheck(prop_enum(),  [{to_file, user}, {numtests, 1000}]).

prop_enum() ->
    ?TARGET_STRATEGY(target_enumerate,
		    ?FORALL(I, ?TARGET(X, integer(0,X)),
		     	     begin
		     		 io:format("I: ~p~n", [I]),
		     		 ?MAXIMIZE(I,X)
		     	     end)).

sa_test() ->
    proper:quickcheck(prop_sa(),  [{to_file, user}, {numtests, 1000}]).

-define(NUMTESTS(N, P), proper:numtests(N, P)).

prop_sa() ->
    ?TARGET_STRATEGY(target_sa,
		    ?NUMTESTS(
		       10000,
		       ?FORALL(I, ?TARGET(X, exactly(X), target_sa:integer()),
			       begin
				   io:format("I: ~p~n", [I]),
				   ?MAXIMIZE(-I)
			       end))).
