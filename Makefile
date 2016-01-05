PROJECT = dialyzeit

COMPILE_FIRST := di_bad di_good

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Wunmatched_returns

include erlang.mk
