#!/bin/sh
cd "$(dirname $0)"

# Commands
INSTRUMENT="gnatcov instrument --level stmt+mcdc"
BUILD="gprbuild --implicit-with=gnatcov_rts --src-subdirs=gnatcov-instr"
COVERAGE="gnatcov coverage --timezone utc --output-dir reports --non-coverable --level stmt+mcdc --annotate sarif,shtml+"

# Build RTS
if [[ ! -e rts ]] then
   gnatcov setup --prefix=rts
fi

# Common variables
GPR_PROJECT_PATH=$GPR_PROJECT_PATH:"$PWD"/rts/share/gpr
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"$PWD"/rts/lib

# Instrument with focus on support library
#$INSTRUMENT -Pbindings_cov --projects dbusada_support || exit -1

# Build and run instrumented code
$BUILD -Pbindings_cov || exit -1

rm *.srctrace
./bin/bindings test
#./run.sh || exit -1

# Generate coverage report
rm -r reports
$COVERAGE -Pbindings_cov *.srctrace || exit -1
