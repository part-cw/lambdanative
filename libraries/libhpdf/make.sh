
PKGURL=http://libharu.org/files/libharu-2.2.1.tar.gz
PKGHASH=b75ec6052b8d72aa7f23d67adcdf9df4847b64ca

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=--host=`echo "${SYS_ARCH}-" | cut -f 1 -d "-"`
fi

package_configure $EXTRACONF  --without-zlib --without-png --disable-shared

mv Makefile tmp
cat tmp | sed "s/-arch i386//g;s/-arch x86_64/-m32/" > Makefile

package_make
package_make install
package_cleanup

#eof
