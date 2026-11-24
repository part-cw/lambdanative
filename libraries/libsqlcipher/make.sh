PKGURL=https://github.com/sqlcipher/sqlcipher/archive/refs/tags/v4.20.0.tar.gz
PKGHASH=745fddd5e310b5d855a5f960fc92171d071ef907

package_download $PKGURL $PKGHASH
package_patch
#rmifexists $SYS_PREFIX/include/sqlite3*.h

case $SYS_PLATFORM in
ios)
  EXTRACONF=--host=x86_64-apple-darwin
;;
android)
  EXTRACONF=--host=arm-eabi
  export LIBS='-llog'
;;
win32*)
  EXTRACONF=--host=mingw32
;;
linux)
  EXTRACONF=--host=i386-linux
;;
*)
  EXTRACONF=
;;
esac

package_configure --disable-tcl --disable-shared --with-tempstore=yes CFLAGS=\"-DSQLITE_HAS_CODEC -DSQLITE_EXTRA_INIT=sqlcipher_extra_init -DSQLITE_EXTRA_SHUTDOWN=sqlcipher_extra_shutdown\" LDFLAGS=\"-L$SYS_PREFIX/lib -lcrypto\" $EXTRACONF

if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=
  package_make
else
  package_make lib
fi

mkdir -p $SYS_PREFIX/include/sqlcipher
cp libsqlite3.a $SYS_PREFIX/lib/libsqlcipher.a
cp sqlite3.h $SYS_PREFIX/include/sqlcipher
cp sqlite3ext.h $SYS_PREFIX/include/sqlcipher
assertfile "$SYS_PREFIX/lib/libsqlcipher.a" "SQLCipher did not install its static library"

package_cleanup

#eof
