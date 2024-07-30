#!/bin/sh
function cycle_client {
   rm -r examples/generated/client
   mkdir -p examples/generated/client
   ./dbus_binding_generator_ada -client "$@" > examples/generated/client/output.ada || exit -1
   gnatchop examples/generated/client/output.ada examples/generated/client || exit -1
   gnatpp examples/generated/client/*.ads || exit -1
}

function cycle_server {
   rm -r examples/generated/server
   mkdir -p examples/generated/server
   ./dbus_binding_generator_ada -server "$@" > examples/generated/server/output.ada || exit -1
   gnatchop examples/generated/server/output.ada examples/generated/server || exit -1
   gnatpp examples/generated/server/*.ads || exit -1
}

function cycle_types {
   mkdir examples/generated
   rm examples/generated/*.*
   ./dbus_binding_generator_ada -types "$@" > examples/generated/output.ada || exit -1
   gnatchop examples/generated/output.ada examples/generated || exit -1
   gnatpp examples/generated/*.ads || exit -1
}

gprbuild || exit -1

#cycle_client "$@"
cycle_server "$@"
#cycle_types "$@"
