#!/bin/sh
die() {
   rm data
   exit -1
}

cd "$(dirname $0)"
rm -r generated
mkdir generated

ln -s ../data .

CLIENT_SPECS="../specs/org.mpris.MediaPlayer2.xml ../specs/org.freedesktop.DBus.xml"
SERVER_SPECS="../specs/org.mpris.MediaPlayer2.xml"

../bin/dbus_binding_generator_ada -client $CLIENT_SPECS > generated/client.ada || die
../bin/dbus_binding_generator_ada -server $SERVER_SPECS > generated/server.ada || die
../bin/dbus_binding_generator_ada -types $CLIENT_SPECS $SERVER_SPECS > generated/types.ada || die

gnatchop generated/client.ada generated/ || die
gnatchop generated/server.ada generated/ || die
gnatchop generated/types.ada generated/ || die

gnatpp generated/*.ads || die
rm generated/*.ada data || die
