
package_from_source *.c *.h

minizip_defs=
if [ $SYS_PLATFORM = macosx ] || [ $SYS_PLATFORM = ios ] || [ $SYS_PLATFORM = android ]; then
  minizip_defs=-DUSE_FILE32API
fi

rm *.o 2> /dev/null
veval "$SYS_CC -c *.c $minizip_defs -I$SYS_PREFIX/include"
asserterror $? "compilation failed"
veval "$SYS_AR ru libminizip.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB libminizip.a"
asserterror $? "compilation failed"

cp libminizip.a $SYS_PREFIX/lib
cp minizip.h $SYS_PREFIX/include

package_cleanup

#eof
