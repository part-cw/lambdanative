PKGURL=https://github.com/webmproject/libwebp/archive/refs/tags/v1.6.0.tar.gz
PKGHASH=6a5da51c23c8340e44a70421a5ef8bb1ae805ad2

package_download $PKGURL $PKGHASH

veval "./autogen.sh"
asserterror $? "failed to generate configure script"

rmifexists $SYS_PREFIX/include/webp

if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --disable-shared --enable-static --enable-dependency-tracking --enable-pic --disable-libwebpmux --disable-libwebpdemux --disable-gl --disable-gif --disable-png --disable-tiff --disable-jpeg --disable-sdl --disable-wic --disable-threading

package_make

package_make install

package_cleanup

#eof
