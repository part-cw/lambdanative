VERSION=3210000
PKGURL="https://www.sqlite.org/2017/sqlite-amalgamation-${VERSION}.zip"
PKGHASH=ebe33c20d37a715db95288010c1009cd560f2452

package_download $PKGURL $PKGHASH

echo "==> Building library source..."
$SYS_CC -c -I. -I$SYS_PREFIX/include sqlite3.c
$SYS_AR ru $SYS_PREFIX/lib/libsqlite.a sqlite3.o  2> /dev/null
$SYS_RANLIB $SYS_PREFIX/lib/libsqlite.a  
cp sqlite3.h $SYS_PREFIX/include
cp sqlite3ext.h $SYS_PREFIX/include

package_cleanup

#eof
