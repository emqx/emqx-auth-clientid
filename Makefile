## shallow clone for speed

REBAR_GIT_CLONE_OPTIONS += --depth 1
export REBAR_GIT_CLONE_OPTIONS

REBAR = rebar3
all: compile

compile:
	$(REBAR) compile

clean: distclean

ct: compile
	$(REBAR) as test ct

eunit: compile
	$(REBAR) as test eunit

cover:
	$(REBAR) cover

xref:
	$(REBAR) xref

distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock
