#!/bin/sh
CMD="${1:-../../bin/dbus_binding_generator_ada}"

die() {
   exit -1
}

cd "$(dirname $0)"
rm -r generated
mkdir generated

CLIENT_SPECS="../testdata/test.interface.xml"
SERVER_SPECS="../testdata/test.interface.xml"

$CMD -client $CLIENT_SPECS > generated/client.ada || die
$CMD -server $SERVER_SPECS > generated/server.ada || die
$CMD -types $CLIENT_SPECS $SERVER_SPECS > generated/types.ada || die

gnatchop generated/client.ada generated/ || die
gnatchop generated/server.ada generated/ || die
gnatchop generated/types.ada generated/ || die

gnatpp generated/*.ads || die
rm generated/*.ada || die
