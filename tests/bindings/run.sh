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
   #echo $RESULT
   test $r -eq 0
}

#####################
# Generate Bindings #
#####################
cd "$(dirname $0)"

if [[ ! -d generated/ ]] then
   ./generate.sh "$(realpath ../../dbus_binding_generator_ada)" || die
fi;

gprbuild -j0 || die

#####################
# Test Connectivity #
#####################
echo Start Service
./bin/bindings_server &
PID=$!
sleep 0.5

echo Check connectivity
cmd / com.example.Interface.TestEmpty || die

##################################
# Invalid Objects and Interfaces #
##################################
echo Invalid Objects and Interfaces

echo Invalid object
cmd /invalid/object com.example.Interface.TestEmpty && die

echo Invalid interface
cmd / invalid.Interface.InvalidName && die

echo Invalid method
cmd / com.example.Interface.InvalidMethod && die

echo Wrong interface for object
cmd /Interior com.example.Interface.TestEmpty && die

echo Wrong signature
cmd / com.example.Interface.TestEmpty string:test && die

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
tgt=com.example.Interface

echo $obj $iface "(" $tgt ")"

echo Write nonexistent interface
cmd $obj $iface.Set string:invalid.Interface string:InvalidProperty variant:string:Hello && die

echo Read nonexistent interface
cmd $obj $iface.Get string:invalid.Interface string:InvalidProperty && die

echo Read all nonexistent interface
cmd $obj $iface.GetAll string:invalid.Interface && die

echo Write nonexistent property
cmd $obj $iface.Set string:$tgt string:InvalidProperty variant:string:Hello && die

echo Read nonexistent property
cmd $obj $iface.Get string:$tgt string:InvalidProperty && die

echo Write readonly property
cmd $obj $iface.Set string:$tgt string:TestPropertyReadOnly variant:string:Hello && die

echo Read writeonly property
cmd $obj $iface.Get string:$tgt string:TestPropertyWriteOnly && die

echo Write property with invalid type
cmd $obj $iface.Set string:$tgt string:TestPropertyWriteOnly int32:0 && die

echo Write property with invalid variant contents
cmd $obj $iface.Set string:$tgt string:TestPropertyWriteOnly variant:int32:0 && die

####################
# Ada Client Tests #
####################
echo "Ada client tests"
./bin/bindings_client || die

# The server should have shut down
kill $PID 2>/dev/null && die

###################
# Ada Mixed Tests #
###################
echo "Ada mixed tests"
./bin/bindings_mixed || die

echo PASS
