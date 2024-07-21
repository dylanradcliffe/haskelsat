# check results with known SAT solver
# Use: util/compare.sh file.cnf
EXEC=`cabal list-bin haskelsat`
FILE=$1
RES=`cat $FILE | $EXEC |perl -pe 's/,/ 0 /g;s/^Sat \[(.*)\]/\1 0/'`
echo $RES
cat $FILE <(echo $RES) | glucose-simp 2>/dev/null |grep ^s

