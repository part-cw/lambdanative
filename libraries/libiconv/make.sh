
package_from_source *.c *.h

rm *.o 2> /dev/null
veval "$SYS_CC -c *.c -I$SYS_PREFIX/include"
asserterror $? "compilation failed"
veval "$SYS_AR ru libiconv.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB libiconv.a"
asserterror $? "compilation failed"

cp libiconv.a $SYS_PREFIX/lib
cp iconv.h $SYS_PREFIX/include

package_cleanup

#eof
