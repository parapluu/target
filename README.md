# Target - Targeted Property-Based Testing

Target adds search-based software testing to PropEr. This adds search
strategies to the input generation process and helps finding bugs faster.

## Simple Example

```Erlang
-include_lib("proper/include/proper.hrl").
-include_lib("target/include/target.hrl").

prop_example_target() ->
  ?TARGET_STRATEGY(
    target_sa,
      ?FORALL(X, ?TARGET(target_sa:float(0.0, 10000.0)),
              begin
		?MAXIMIZE(X),
		X < 9999.0
	      end)).

%% prop_example() ->
%%  ?FORALL(X, float(0.0, 10000.0), X < 9999.0).
```

Target will be able to falsify properties like these much faster than vanilla
PropEr

```Erlang
put(target_sa_steps, 10000).
proper:quickcheck(prop_example_target(), 10000).
```
