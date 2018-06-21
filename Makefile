REBAR = rebar3

.PHONY: all compile escriptize test clean
.PHONY: test eunit xref dialyze
.PHONY: release release_minor release_major release_patch

all: compile escriptize

compile:
	@$(REBAR) compile

escriptize:
	@$(REBAR) escriptize

clean:
	@find . -name "*~" -exec rm {} \;
	@$(REBAR) clean

test: compile xref eunit

eunit: all
	ERL_FLAGS="-sname eunit" $(REBAR) eunit
	@$(REBAR) cover

xref: all
	@$(REBAR) xref

dialyze:
	@$(REBAR) dialyzer

release_major: test
	./bin/release.sh major

release_minor: test
	./bin/release.sh minor

release_patch: test
	./bin/release.sh patch

release: relase_patch

publish:
	@$(REBAR) hex publish
