#!/bin/sh
rm test/generated/*.*
gprbuild && ./dbus_binding_generator_ada "$@" > test/generated/output.ada && gnatchop test/generated/output.ada test/generated/ && gnatpp test/generated/*.ads
