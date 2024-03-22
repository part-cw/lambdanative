VERSION=3450200
PKGURL="https://www.sqlite.org/2024/sqlite-amalgamation-${VERSION}.zip"
PKGHASH=190c19d01e94298470ac3d7b17e6bb391b6c953a

package_download $PKGURL $PKGHASH
package_patch

echo "==> Building library source..."
$SYS_CC -c -I. -I$SYS_PREFIX/include sqlite3.c
assertfile sqlite3.o
$SYS_AR ru $SYS_PREFIX/lib/libsqlite.a sqlite3.o  2> /dev/null
$SYS_RANLIB $SYS_PREFIX/lib/libsqlite.a  
cp sqlite3.h $SYS_PREFIX/include
cp sqlite3ext.h $SYS_PREFIX/include

package_cleanup

#eof
