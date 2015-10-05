
package_from_source *.c *.h

rm *.o 2> /dev/null
cp glob.h $SYS_PREFIX/include
veval "$SYS_CC -c *.c -I$SYS_PREFIX/include"
asserterror $? "compilation failed"
veval "$SYS_AR ru libglob.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB libglob.a"
asserterror $? "compilation failed"

cp libglob.a $SYS_PREFIX/lib

package_cleanup

#eof
