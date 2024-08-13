#!/bin/sh
GNAT_INCLUDE="$(dirname $(readlink $(which gnat)))/../lib/gcc/$(gcc -dumpmachine)/$(LC_ALL=C gnat --version | head -n1 | cut -d' ' -f2)/adainclude/"
rr replay -- --eval-command="directory $GNAT_INCLUDE" --eval-command="set substitute-path /build/dbus-1.14.10/ $HOME/src/dbus"
#LD_PRELOAD=./debug/dbus.debug.so gdb --eval-command="directory $GNAT_INCLUDE" --eval-command="set substitute-path /build/dbus-1.14.10/ $HOME/src/dbus"
