PROJECT = imperial

HAS_MERL := $(shell erl -eval 'erlang:display(list_to_integer(erlang:system_info(otp_release)) >= 18), halt().'  -noshell)

ifneq (true,$(HAS_MERL))
DEPS = merl
else
MERL_OPTS = -DHAS_MERL
ERLC_OPTS = $(MERL_OPTS)
TEST_ERLC_OPTS = $(MERL_OPTS)
DIALYZER_OPTS = $(MERL_OPTS)
endif

include erlang.mk
