-module(good_child).

-behaviour(bad_behaviour).
-behaviour(good_behaviour).

-export([good/0, bad/0]).

good() -> good.

bad() -> not_bad.
