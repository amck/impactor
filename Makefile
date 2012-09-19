REBAR:=rebar
APPNAME:=impactor
APPSRC:=apps/$(APPNAME)/src/$(APPNAME).app.src
VERSION:=$(shell grep vsn $(APPSRC) | awk -F\" '{print $$2}')
ARCHIVE_DIR:=/home/public/builds/$(APPNAME)
ARCHIVE_NAME:=$(APPNAME)_$(VERSION)

.PHONY: all erl update release test clean doc release-dev release-qa release-uat release-prod archive clean-rel
all:  erl

erl:
	$(REBAR) compile

release: all
	$(REBAR) generate
	tar jcf rel/$(ARCHIVE_NAME).tbz2 -C rel $(APPNAME)

test: all
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	-rm -rvf ebin doc .eunit

clean-rel:
	-rm -rvf rel/$(APPNAME)

archive:
	mkdir -p $(ARCHIVE_DIR)
	cp -u rel/$(ARCHIVE_NAME).tbz2 $(ARCHIVE_DIR)

doc:
	$(REBAR) doc
	erl -pa ./apps/$(APPNAME)/ebin ./ebin ./deps/*/ebin ./deps/*/deps/*/ebin ./deps/*/include -debug -name $(APPNAME)@127.0.0.1 -config ./rel/files/app.config -eval "application:start($(APPNAME))."
