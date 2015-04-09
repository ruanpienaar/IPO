ERL					?= erl
ERLC				= erlc
EBIN_DIRS		:= $(wildcard deps/*/ebin)
APPS				:= $(shell dir apps)
REL_DIR     = rel
NODE				= ipo
REL					= ipo
SCRIPT_PATH  := $(REL_DIR)/$(NODE)/bin/$(REL)

.PHONY: rel offline compile get-deps update-deps test clean deep-clean rabbit

rel: compile
	@cd rel/in; rebar generate -vf
	@cd rel/out; rebar generate -vf 
	@cd rel/out_buff; rebar generate -vf 
	@cd rel/proc; rebar generate -vf
	@cd rel/proc_buff; rebar generate -vf
	@cd rel; [ -d rabbitmq-codegen ] || git clone https://github.com/rabbitmq/rabbitmq-codegen.git
	@cd rel; [ -d rabbitmq-server ] || git clone https://github.com/rabbitmq/rabbitmq-server.git && cd rabbitmq-server; make all
	@cd rel; cp -r rabbitmq-server proc_buff/proc_buff
	@cd rel; cp -r rabbitmq-server out_buff/out_buff
	
rabbit:
	git clone https://github.com/rabbitmq/rabbitmq-codegen.git
	git clone https://github.com/rabbitmq/rabbitmq-server.git && cd rabbitmq-server; make

compile: get-deps update-deps
	@rebar compile

get-deps:
	@rebar get-deps

update-deps:
	@rebar update-deps

clean:
	@rm -rf rel/in/in
	@rm -rf rel/out/out
	@rm -rf rel/out_buff/out_buff
	@rm -rf rel/proc/proc
	@rm -rf rel/proc_buff/proc_buff
	@find apps -name "*.beam" | xargs rm
	@find apps -name "*.app" | xargs rm
	@rebar clean

deep-clean: clean
	@rebar delete-deps

setup_dialyzer:
	dialyzer --build_plt --apps erts kernel stdlib mnesia compiler syntax_tools runtime_tools crypto tools inets ssl webtool public_key observer
	dialyzer --add_to_plt deps/*/ebin

dialyzer: compile
	dialyzer */apps/*/ebin

doc:
	rebar skip_deps=true doc
	for app in $(APPS); do \
		cp -R apps/$${app}/doc doc/$${app}; \
	done;

analyze: checkplt
	@rebar skip_deps=true dialyze

buildplt:
	@rebar skip_deps=true build-plt

checkplt: buildplt
	@rebar skip_deps=true check-plt