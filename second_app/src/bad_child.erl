-module(bad_child).

-behaviour(good_behaviour).
-behaviour(bad_behaviour).

-export([good/0, bad/0]).

%% @doc This function correclty implements good_behaviour.
good() -> good.

%% @doc This function incorreclty implements bad_behaviour.
%%      That fact goes unnoticed by dialyzer here.
bad() -> not_bad.
