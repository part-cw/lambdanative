PKGURL=http://download.savannah.gnu.org/releases/freetype/freetype-2.5.5.tar.gz
PKGHASH=884830e13a4ebd780150697bab7e172e902194c9

package_download $PKGURL $PKGHASH
package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared --without-png --without-harfbuzz --without-zlib --without-bzip2

package_make
package_make install
package_cleanup

#eof
