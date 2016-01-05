%% This file has to be included after proper.hrl
%% There is unfortunatley no easy way to check this and print a warning if the the
%% include is missing. Also double includes are not ignored.

%% Define a target
-define(NAMED_TARGET(TargetArg, Gen, Opts), target:targeted(??TargetArg, fun(TargetArg) -> Gen end, Opts)).
-define(NAMED_TARGET(TargetArg, Gen), ?NAMED_TARGET(TargetArg, Gen, [])).

-define(TARGET(Opts), target:targeted(make_ref(), fun(X) -> X end, Opts)).
-define(TARGET(), ?TARGET([])).

%% Define the search
%%   global feedback
-define(MAXIMIZE(Fitness), target:adjust(Fitness, inf)).
-define(MAXIMIZE(Fitness, Target), target:adjust(Fitness, inf, ??Target)).
%%   feedback for single targets
-define(MAXIMIZE_UNTIL(Fitness, Threshold), target:adjust(Fitness, Threshold)).
-define(MAXIMIZE_UNTIL(Fitness, Target, Threshold), target:adjust(Fitness, Threshold, ??Target)).

%% Define a strategy
-define(TARGET_STRATEGY(Strategy, Prop), target_strategy:use_strategy(Strategy, Prop)).

%% Util
-define(FORALL_SA(X, RawType, Prop), target_strategy:use_strategy(target_sa, proper:forall(RawType,fun(X) -> Prop end))).
