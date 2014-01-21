#!/bin/sh
exec erl -pa ebin deps/*/ebin -config etc/app.config -s actordb_core -args_file etc/vm.args
