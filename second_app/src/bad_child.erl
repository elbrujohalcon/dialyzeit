-module(bad_child).

-behaviour(good_behaviour).
-behaviour(bad_behaviour).

-export([good/0, bad/0]).

good() -> good.

bad() -> not_bad.
