PKGURL=https://download.osgeo.org/libtiff/tiff-4.7.1.tar.gz
PKGHASH=3677c549c68801c28bf870f174f0cdf408377b8d

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --disable-shared --enable-static --disable-tools --disable-tests --disable-docs --disable-contrib

package_make

package_make install

package_cleanup

#eof
