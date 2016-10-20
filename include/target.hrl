%% This file has to be included after proper.hrl
%% There is unfortunately no easy way to check this and print a warning
%% if the include is missing.  Also double includes are not ignored.

%% Define a target
-define(NAMED_TARGET(TargetArg, Gen), ?NAMED_TARGET(TargetArg, Gen, #{})).
-define(NAMED_TARGET(TargetArg, Gen, TMap), target:targeted(??TargetArg, fun(TargetArg) -> Gen end, TMap)).

-define(TARGET(), ?TARGET(#{})).
-define(TARGET(TMap), target:targeted(make_ref(), fun(X) -> X end, TMap)).

%% Define the search
%%   global feedback
-define(MAXIMIZE(Fitness), target:adjust(Fitness, inf)).
-define(MAXIMIZE(Fitness, Target), target:adjust(Fitness, inf, ??Target)).
%%   feedback for single targets
-define(MAXIMIZE_UNTIL(Fitness, Threshold), target:adjust(Fitness, Threshold)).
-define(MAXIMIZE_UNTIL(Fitness, Target, Threshold), target:adjust(Fitness, Threshold, ??Target)).

%% Define a strategy
-define(TARGET_STRATEGY(Strat, Prop), target_strategy:use_strategy(Strat, Prop)).

%% Util
-define(FORALL_SA(X, RawType, Prop), target_strategy:use_strategy(target_sa, proper:forall(RawType,fun(X) -> Prop end))).
