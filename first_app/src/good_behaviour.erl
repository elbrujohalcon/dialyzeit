-module(good_behaviour).

-callback good() -> good.

-export([good/0]).

%% @doc This function overlaps with the callback but it's not a problem for
%%      dialyzer
good() -> good.
