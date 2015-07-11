%% This file has to be included after proper.hrl
%% There is unfortunatley no easy way to check this and print a warning if the the 
%% include is missing. Also double includes are not ignored.

%% Define a target
-define(TARGET(TargetArg, Gen, Opts), target:targeted(fun(TargetArg) -> Gen end, Opts)).
-define(TARGET(TargetArg, Gen), ?TARGET(TargetArg, Gen, [])).

%% Define the search
-define(MAXIMIZE(Fitness, Gen), target:adjust(Gen, Fitness, inf)).
-define(MAXIMIZE(Fitness, Gen, Threshold), target:adjust(Gen, Fitness, Threshold)).
-define(MINIMIZE(Fitness, Gen), target:adjust(Gen, -Fitness, inf)).
-define(MINIMIZE(Fitness, Gen, Threshold), target:adjust(Gen, -Fitness, -Threshold)).

%% Define a strategy
-define(TARGET_STRATEGY(Strategy, Prop), target_strategy:use_strategy(Strategy, Prop)).

%% Utility
-define(TARGET_STORRAGE_GET(Hash, Opts), target_strategy:get_target(Hash, Opts)).
-define(TARGET_STORRAGE_SET(Hash, State), target_strategy:update_target(Hash, State)).



%% %% Envisioned ussage
%% prop() ->
%%     ?TARGET_STRATEGY(enumerate,
%% 		     ?FORALL(I, ?TARGET(X, integer(0,X)),
%% 			     ?MAXIMIZE(I))).
