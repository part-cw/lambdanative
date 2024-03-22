VERSION=v4.5.6
PKGURL="https://github.com/sqlcipher/sqlcipher/archive/refs/tags/${VERSION}.zip"
PKGHASH=76eb4248d50a79115a48acee415d3885259fcfd5

package_download $PKGURL $PKGHASH
package_patch
rmifexists $SYS_PREFIX/include/sqlite3*.h

package_configure --disable-tcl --disable-shared --enable-tempstore=always CFLAGS="-DSQLITE_HAS_CODEC"

package_make
package_make install

package_cleanup

#eof
