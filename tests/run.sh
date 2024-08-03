die() {
   echo FAIL
   exit -1
}

cd "$(dirname "$0")"

for i in *
do
   if [[ -e $i/run.sh ]] then
      $i/run.sh || die
   fi
done
echo PASS
