#!/bin/sh
RESULT=""

die() {
   echo $RESULT
   echo FAIL
   exit -1
}

cd "$(dirname $0)"
mkdir testdir
cd testdir || die

####################
# Run Binder Tests #
####################
../../binder/run.sh $(realpath ../../../bin/dbus_binding_generator_ada_cov) || die

#####################
# Run Binding Tests #
#####################
BUILD_MODE=coverage ../../bindings/run.sh die

##################
# Produce Report #
##################
echo Produce Report
RESULT="$(lcov -c -d ../../../obj/coverage -d ../../../supportlib/obj/coverage -o coverage.run 2>&1)" || die
RESULT="$(lcov -e coverage.run "$(realpath $PWD/../../..)/*" -o coverage.run.filtered)" || die
RESULT="$(genhtml -o html/ coverage.run.filtered)" || die

echo PASS
