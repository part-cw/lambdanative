PKGURL=https://gmplib.org/download/gmp/gmp-6.3.0.tar.xz
PKGHASH=b4043dd2964ab1a858109da85c44de224384f352

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared

package_make

package_make install

package_cleanup

#eof
