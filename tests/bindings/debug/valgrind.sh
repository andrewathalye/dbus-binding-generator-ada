#!/bin/sh
LD_PRELOAD=./debug/dbus.debug.so valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=out.txt "$@"
#LD_PRELOAD=./debug/dbus.debug.so valgrind --leak-check=full --track-origins=yes --log-file=out.txt --vgdb-error=0 "$@" &
#gdb "$@"

