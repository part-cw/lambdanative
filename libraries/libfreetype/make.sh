
PKGURL=http://download.savannah.gnu.org/releases/freetype/freetype-2.5.3.tar.gz
PKGHASH=d4a17b42505b23dab022f877e1849940aa3b64f3

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
