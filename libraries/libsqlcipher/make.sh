VERSION=v4.5.6
PKGURL="https://github.com/sqlcipher/sqlcipher/archive/refs/tags/${VERSION}.zip"
PKGHASH=76eb4248d50a79115a48acee415d3885259fcfd5

package_download $PKGURL $PKGHASH
package_patch
rmifexists $SYS_PREFIX/include/sqlite3*.h

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

if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=
fi

package_configure --disable-tcl --disable-shared --enable-tempstore=always CFLAGS="-DSQLITE_HAS_CODEC" $EXTRACONF

package_make
package_make install

package_cleanup

#eof
