#!/bin/sh
exec erl -pa ebin deps/*/ebin apps/myactor/ebin -config etc/app.config -s actordb_core -args_file etc/vm.args
