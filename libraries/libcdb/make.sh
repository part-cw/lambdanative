
PKGURL=http://www.corpit.ru/mjt/tinycdb/tinycdb-0.78.tar.gz
PKGHASH=ade42ee1e7c56f66a63cb933206c089b9983adba

package_download $PKGURL $PKGHASH

srcs=`ls -1 cdb_*.c`

rm *.o 2> /dev/null
veval "$SYS_CC -D_FILE_OFFSET_BITS=64 -c $srcs"
asserterror $? "compilation failed"
veval "$SYS_AR ru libcdb.a *.o"
asserterror $? "compilation failed"
veval "$SYS_RANLIB libcdb.a"
asserterror $? "compilation failed"

cp libcdb.a $SYS_PREFIX/lib
cp cdb.h $SYS_PREFIX/include

package_cleanup

#eof
