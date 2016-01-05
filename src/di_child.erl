-module(di_child).

-behaviour(di_good).
-behaviour(di_bad).

-export([good/0, bad/0]).

good() -> good.

bad() -> not_bad.
