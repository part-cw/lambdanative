PKGURL=https://archive.ubuntu.com/ubuntu/pool/main/t/tinycdb/tinycdb_0.81.orig.tar.gz
PKGHASH=759f89cc7051d932ebe67cca9d94457be8c04b5d

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
