#!/bin/sh

RESULT=""

fail() {
   echo $RESULT
   echo "FAIL"
   exit -1
}

cmd() {
   RESULT=$(dbus-send --print-reply --dest=org.mpris.MediaPlayer2.test "$@" 2>&1)
}

RESULT="not yet implemented"
fail

obj="/org/mpris/MediaPlayer2"
iface="org.freedesktop.DBus.Properties"

echo Wrong object
cmd /test/test/Test test.test.Test && fail

echo Wrong interface
cmd $obj test.test.Test && fail

echo Wrong method
cmd $obj $iface.Test && fail

echo Get with wrong arguments
cmd $obj $iface.Get int32:1 && fail

echo Get non-existent property
cmd $obj $iface.Get string:test.test.Test string:Test && fail

echo Get
cmd $obj $iface.Get string:org.mpris.MediaPlayer2 string:Identity || fail

echo GetAll
cmd $obj $iface.GetAll string:org.mpris.MediaPlayer2.Player || fail

echo Set on non-existent property
cmd $obj $iface.Set string:test.test.Test string:Test variant:int32:0 && fail

echo Set on read-only property
cmd $obj $iface.Set string:org.mpris.MediaPlayer2.Player string:PlaybackStatus variant:string:Test && fail

echo Set with wrong property type
cmd $obj $iface.Set string:org.mpris.MediaPlayer2.Player string:Volume variant:int32:1 && fail

echo Set
cmd $obj $iface.Set string:org.mpris.MediaPlayer2.Player string:Volume variant:double:0.5 || fail

echo Quit
cmd $obj org.mpris.MediaPlayer2.Quit || fail

echo Quit after Quit
cmd $obj org.mpris.MediaPlayer2.Quit && fail

echo PASS
