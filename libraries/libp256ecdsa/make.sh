
package_from_source *.c *.h

mkdir -p $SYS_PREFIX/include/p256ecdsa
cp *.h $SYS_PREFIX/include/p256ecdsa

rm *.o 2> /dev/null
veval "$SYS_CC -c *.c -I$SYS_PREFIX/include"
asserterror $? "compilation failed"
veval "$SYS_AR ru libp256ecdsa.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB libp256ecdsa.a"
asserterror $? "compilation failed"

cp libp256ecdsa.a $SYS_PREFIX/lib

package_cleanup

#eof
