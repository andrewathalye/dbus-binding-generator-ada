CMD="${1:-../../dbus_binding_generator_ada}"
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
mkdir binder_testdir
cd binder_testdir

mkdir data
cp ../../data/introspect.xsd data

################
# BINDER TESTS #
################
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
cmd -client ../data/test.interface.xml || die
cmd -server ../data/test.interface.xml || die
cmd -types ../data/test.interface.xml || die

echo Erroneous Specifications
cmd -types ../data/test.error_complexdictkey.xml && die
cmd -types ../data/test.error_schemavalidation.xml && die

echo Erroneous Schema
cp ../data/invalid.xsd data/introspect.xsd
cmd -types ../data/test.interface.xml && die

echo PASS
