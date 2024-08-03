CMD="${1:-../../bin/dbus_binding_generator_ada}"

RESULT=""

die() {
   echo $RESULT
   echo FAIL
   exit -1
}

cmd() {
   RESULT="$($CMD $@ 2>&1)"
}

cd "$(dirname $0)"

################
# BINDER TESTS #
################
echo No Arguments
cmd && die

echo No input files
cmd -types && die

echo Incorrect Arguments
cmd --invalid-argument && die

echo Option Delimiter
RESULT=`$CMD -- -client 2>&1 | grep client` || die

echo Help
cmd --help && die
cmd -help && die

echo Fake File
cmd -types ./doesnotexist && die

echo Test Specification
cmd -client ../testdata/test.interface.xml || die
cmd -server ../testdata/test.interface.xml || die
cmd -types ../testdata/test.interface.xml || die

echo Erroneous Specifications
cmd -types ../testdata/test.error_complexdictkey.xml && die
cmd -types ../testdata/test.error_schemavalidation.xml && die
cmd -types ../testdata/test.error_propertyplacement.xml && die

echo PASS
