VERSION=3540100
PKGURL="https://www.sqlite.org/2026/sqlite-amalgamation-${VERSION}.zip"
PKGHASH=dbe0fa9aaeac8ad29fc3e094792896d556a57163

package_download $PKGURL $PKGHASH
package_patch

echo "==> Building library source..."
cp sqlite3.h $SYS_PREFIX/include
cp sqlite3ext.h $SYS_PREFIX/include

$SYS_CC -c -I. -I$SYS_PREFIX/include sqlite3.c
assertfile sqlite3.o
$SYS_AR ru $SYS_PREFIX/lib/libsqlite.a sqlite3.o  2> /dev/null
$SYS_RANLIB $SYS_PREFIX/lib/libsqlite.a  

package_cleanup

#eof
