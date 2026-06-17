PKGURL=https://github.com/LibRaw/LibRaw/archive/refs/tags/0.22.1.tar.gz
PKGHASH=9f4c9811f91d11296c755d2f8f229a2aabae63a6

package_download $PKGURL $PKGHASH

veval "autoreconf -fi"
asserterror $? "failed to generate configure script"

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure --enable-static --disable-shared --enable-jpeg --enable-zlib --enable-lcms --disable-examples

package_make

package_make install

package_cleanup

#eof
