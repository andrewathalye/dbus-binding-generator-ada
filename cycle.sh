#!/bin/sh
rm -r examples/generated
mkdir -p examples/generated/client
mkdir -p examples/generated/server

gprbuild || exit -1
./dbus_binding_generator_ada -client "$@" > examples/generated/client/output.ada || exit -1
./dbus_binding_generator_ada -server "$@" > examples/generated/server/output.ada || exit -1

gnatchop examples/generated/client/output.ada examples/generated/client || exit -1
gnatchop examples/generated/server/output.ada examples/generated/server || exit -1

gnatpp examples/generated/client/*.ads || exit -1
gnatpp examples/generated/server/*.ads || exit -1
