
package_from_source *.c *.h

mkdir -p $SYS_PREFIX/include/tinysvcmdns
cp *.h $SYS_PREFIX/include/tinysvcmdns

rm *.o 2> /dev/null
veval "$SYS_CC -DNDEBUG -c *.c -I$SYS_PREFIX/include"
asserterror $? "compilation failed"
veval "$SYS_AR ru libtinysvcmdns.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB libtinysvcmdns.a"
asserterror $? "compilation failed"

cp libtinysvcmdns.a $SYS_PREFIX/lib

package_cleanup

#eof
