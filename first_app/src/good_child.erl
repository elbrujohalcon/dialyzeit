-module(good_child).

-behaviour(bad_behaviour).
-behaviour(good_behaviour).

-export([good/0, bad/0]).

%% @doc This function correclty implements good_behaviour.
good() -> good.

%% @doc This function incorreclty implements bad_behaviour.
bad() -> not_bad.
