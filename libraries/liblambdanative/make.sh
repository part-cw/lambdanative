
package_from_source *.c *.h

rm *.o 2> /dev/null
veval "$SYS_CC -fPIC -c *.c -I$SYS_PREFIX/include"
asserterror $? "compilation failed"
veval "$SYS_AR ru liblambdanative.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB liblambdanative.a"
asserterror $? "compilation failed"

cp liblambdanative.a $SYS_PREFIX/lib
cp lambdanative.h $SYS_PREFIX/include

package_cleanup

#eof
