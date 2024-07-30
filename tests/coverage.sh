#!/bin/sh
RESULT=""

die() {
   echo $RESULT
   echo FAIL
   exit -1
}

cd "$(dirname $0)"
mkdir coverage_testdir
cd coverage_testdir

##########################
# Produce Coverage Build #
##########################
echo Produce Coverage Build
gprbuild -j0 -P../../dbus_binding_generator_ada -XBUILD_MODE=coverage --relocate-build-tree || die

####################
# Run Binder Tests #
####################
../binder.sh $(realpath ./dbus_binding_generator_ada) || exit -1

##################
# Produce Report #
##################
echo Produce Report
RESULT="$(lcov -c -d . -o coverage.run 2>&1)" || die
RESULT="$(lcov -e coverage.run "$(realpath $PWD/../..)/*" -o coverage.run.filtered)" || die
RESULT="$(genhtml -o html/ coverage.run.filtered)" || die

echo PASS
