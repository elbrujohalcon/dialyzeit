-module(bad_behaviour).

-callback bad() -> bad.

-export([publicly_bad/0]).

publicly_bad() -> bad().

bad() -> bad.
