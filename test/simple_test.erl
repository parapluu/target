-module(simple_test).
-export([enumerate_test/0]).

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
		     		 ?MAXIMIZE(I,I)
		     	     end)).
