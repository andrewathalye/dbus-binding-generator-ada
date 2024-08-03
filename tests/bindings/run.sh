#!/bin/sh

RESULT=""

PID=

die() {
   kill $PID
   echo $RESULT
   echo "FAIL"
   exit -1
}

cmd() {
   RESULT=$(dbus-send --print-reply --dest=test.Service "$@" 2>&1)
   r=$?
   test $r -eq 0
}

#####################
# Generate Bindings #
#####################
cd "$(dirname $0)"

if [[ ! -d generated/ ]] then
   ./generate.sh || die
fi;

gprbuild -j0 || die

################
# Start Server #
################
./bin/bindings server &
PID=$!
#sleep 0.5

echo Check connectivity
while :
do
   cmd / com.example.ClientServer.Ping >&/dev/null && break
   sleep 0.1
done

##################################
# Invalid Objects and Interfaces #
##################################
echo Invalid Objects and Interfaces

echo Invalid object
cmd /invalid/object com.example.ClientServer.Ping && die

echo Invalid interface
cmd / invalid.Interface.InvalidName && die

echo Invalid method
cmd / com.example.ClientServer.InvalidMethod && die

echo Wrong interface for object
cmd /Interior com.example.ClientServer.Ping && die

echo Wrong signature
cmd / com.example.ClientServer.Ping string:test && die

######################
# /InteriorWithEmpty #
######################
obj="/InteriorWithEmpty"
iface="com.example.InteriorWithEmpty"
echo $obj $iface

echo Call method on empty interface
cmd $obj $iface.InvalidMethod && die

##########
# /Empty #
##########
obj="/Empty"
echo $obj

echo Call method on empty node
cmd $obj invalid.Node.InvalidMethod && die

#####
# / #
#####
obj="/"
echo $obj

###################################
# org.freedesktop.DBus.Properties #
###################################
iface=org.freedesktop.DBus.Properties
tgt=com.example.ClientServer

echo $obj $iface "(" $tgt ")"

echo Write property with invalid type
cmd $obj $iface.Set string:$tgt string:TestPropertyWriteOnly int32:0 && die

kill $PID || die

##################
# Ada Test Suite #
##################
echo "Ada Test Suite: Single"
./bin/bindings test || RESULT="Ada code failed" die
echo "Ada Test Suite: Loop"
./bin/bindings loop >/dev/null || RESULT ="Ada code failed" die

echo PASS
