CMD="${1:-../../../dbus_binding_generator_ada}"
DATA="../../../data"
TESTDATA="../../testdata"

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
mkdir -p testdir/data || die
cd testdir || die

cp -r $DATA/introspect.xsd data/ || die

################
# BINDER TESTS #
################
echo No Arguments
cmd && die

echo No input files
cmd -types && die

echo Incorrect Arguments
cmd --invalid-argument && die

echo Option Delimeter
RESULT=`$CMD -- -client 2>&1 | grep client` || die

echo Help
cmd --help && die
cmd -help && die

echo Fake File
cmd -types ./doesnotexist && die

echo Test Specification
cmd -client $TESTDATA/test.interface.xml || die
cmd -server $TESTDATA/test.interface.xml || die
cmd -types $TESTDATA/test.interface.xml || die

echo Erroneous Specifications
cmd -types $TESTDATA/test.error_complexdictkey.xml && die
cmd -types $TESTDATA/test.error_schemavalidation.xml && die
cmd -types $TESTDATA/test.error_propertyplacement.xml && die

echo Erroneous Schema
cp $TESTDATA/invalid.xsd data/introspect.xsd
cmd -types $TESTDATA/test.interface.xml && die

echo PASS
