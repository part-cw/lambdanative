
VERSION=3080600
PKGURL="https://www.sqlite.org/2014/sqlite-amalgamation-${VERSION}.zip"
PKGHASH=6f6ab5ee38eed46ee0d9a8cd8f1e13f94bba99b3

package_download $PKGURL $PKGHASH

echo "==> Building library source..."
$SYS_CC -c -I. -I$SYS_PREFIX/include sqlite3.c
$SYS_AR ru $SYS_PREFIX/lib/libsqlite.a sqlite3.o  2> /dev/null
$SYS_RANLIB $SYS_PREFIX/lib/libsqlite.a  
cp sqlite3.h $SYS_PREFIX/include
cp sqlite3ext.h $SYS_PREFIX/include

package_cleanup

#eof
