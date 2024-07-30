#!/bin/sh
RESULT=""

die() {
   echo $RESULT
   echo FAIL
   exit -1
}

cmd() {
   RESULT="$(./dbus_binding_generator_ada $@ 2>&1)"
}

cd "$(dirname $0)"
mkdir coverage_build
cd coverage_build

##########################
# Produce Coverage Build #
##########################
echo Produce Coverage Build
gprbuild -j0 -P../../dbus_binding_generator_ada -XBUILD_MODE=coverage --relocate-build-tree || die

mkdir data
cp ../../data/introspect.xsd data

####################
# Run Binder Tests #
####################
echo No Arguments
cmd && die

echo No input files
cmd -types && die

echo Incorrect Arguments
cmd --invalid-argument && die

echo Help
cmd --help && die

echo Fake File
cmd -types ./doesnotexist && die

echo Test Specification
cmd -client ../comprehensive/test.interface.xml || die
cmd -server ../comprehensive/test.interface.xml || die
cmd -types ../comprehensive/test.interface.xml || die

echo Erroneous Specifications
cmd -types ../comprehensive/test.error_complexdictkey.xml && die
cmd -types ../comprehensive/test.error_schemavalidation.xml && die

echo Erroneous Schema
cp ../comprehensive/invalid.xsd data/introspect.xsd
cmd -types ../comprehensive/test.interface.xml && die

##################
# Produce Report #
##################
echo Produce Report
RESULT="$(lcov -c -d . -o coverage.run 2>&1)" || die
RESULT="$(lcov -e coverage.run "$(realpath $PWD/../..)/*" -o coverage.run.filtered)" || die
RESULT="$(genhtml -o html/ coverage.run.filtered)" || die

echo PASS
