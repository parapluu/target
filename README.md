# Target - Targeted Property-Based Testing

Target adds a search-based strategy component to PropEr's input generation
process, which helps in finding counterexamples faster in properties that
aim to maximize (or minimize) the value of some variable of the property.

## Simple Example

```Erlang
-include_lib("proper/include/proper.hrl").
-include_lib("target/include/target.hrl").

prop_example_target() ->
  ?TARGET_STRATEGY(target_sa,
      ?FORALL(X, ?TARGET(target_sa:float(0.0, 10000.0)),
              begin
                ?MAXIMIZE(X),
                X < 9990.0
	      end)).

%% prop_example() ->
%%  ?FORALL(X, float(0.0, 10000.0), X < 9990.0).
```

Target will be able to falsify properties like these much faster than vanilla
PropEr.

```Erlang
put(target_sa_steps, 10000).
proper:quickcheck(prop_example_target(), 10000).
```
