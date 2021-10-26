PROJECT = websocket
PROJECT_DESCRIPTION = Cowboy Websocket example
PROJECT_VERSION = 1

DEPS = cowboy
dep_cowboy_commit = master

# Need to change path based on how its deployed and dir name!
#include balcony/erlang.mk
include erlang.mk 
# Joq Erlang code
c0:
	rm -rf *~ *.dump;
c1:
	rm -rf deps;
	rm -rf */*.beam *~ */*~
git:
	rm -rf */*.beam *~ */*~ *.dump;
	cp src/*.app ebin;
	erlc -o ebin src/*.erl;
balcony:
	rm -rf */*.beam *~ */*~ *.dump *.html;
	cp priv/*.html ebin;
	cp src/*.app ebin;
	erlc -o ebin src/*.erl;
	cp deps/cowboy/ebin/* ebin;
	cp deps/cowlib/ebin/* ebin;
	cp deps/ranch/ebin/* ebin;
	erl -pa ebin -run balcony boot -sname balcony;
build:
	rm -rf */*.beam *~ */*~ *.dump *.html;
	cp priv/*.html ebin;
#	gun
	cp ../gun/ebin/* ebin;
	cp ../gun/deps/cowlib/ebin/* ebin;
#	jsx
	erlc -o ebin ../jsx/src/*.erl;
#	test
	cp test_src/*.app test_ebin;
	erlc -o test_ebin test_src/*.erl;
#	service
	cp src/*.app ebin;
	erlc -o ebin src/*.erl;
	cp deps/cowboy/ebin/* ebin;
	cp deps/cowlib/ebin/* ebin;
	cp deps/ranch/ebin/* ebin;
	rm -rf deps;
	echo done
unit_test:
	rm -rf */*.beam *~ */*~ *.dump *.html;
	cp priv/*.html ebin;
#	gun
	cp ../gun/ebin/* ebin;
	cp ../gun/deps/cowlib/ebin/* ebin;
#	jsx
	erlc -o ebin ../jsx/src/*.erl;
#	test
	cp test_src/*.app test_ebin;
	erlc -o test_ebin test_src/*.erl;
#	service
	cp src/*.app ebin;
	erlc -o ebin src/*.erl;
	cp deps/cowboy/ebin/* ebin;
	cp deps/cowlib/ebin/* ebin;
	cp deps/ranch/ebin/* ebin;
	erl -pa ebin -pa test_ebin\
	    -setcookie cookie\
	    -sname test\
	    -unit_test monitor_node test\
	    -unit_test cluster_id test\
	    -unit_test cookie cookie\
	    -run unit_test start_test test_src/test.config
