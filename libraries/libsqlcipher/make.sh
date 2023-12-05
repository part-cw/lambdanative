VERSION=v4.5.5
PKGURL="https://github.com/sqlcipher/sqlcipher/archive/refs/tags/${VERSION}.zip"
PKGHASH=efec9eb215f0e9905272a20d8a99246e7ddac87d

package_download $PKGURL $PKGHASH
package_patch
rmifexists $SYS_PREFIX/include/sqlite3*.h

package_configure --disable-tcl --disable-shared --enable-tempstore=always CFLAGS="-DSQLITE_HAS_CODEC"

package_make
package_make install

package_cleanup

#eof
