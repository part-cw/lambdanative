PKGURL=https://github.com/mm2/Little-CMS/archive/refs/tags/lcms2.19.1.tar.gz
PKGHASH=9f4c9811f91d11296c755d2f8f229a2aabae63a6

package_download $PKGURL $PKGHASH

veval "./autogen.sh"
asserterror $? "failed to generate configure script"

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure --enable-static --disable-shared --without-jpeg --without-tiff --without-zlib

package_make

package_make install

package_cleanup

#eof
