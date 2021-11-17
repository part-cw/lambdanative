PKGURL=https://download.savannah.gnu.org/releases/freetype/freetype-2.11.0.tar.gz
PKGHASH=e9272ae073e35bb65aa39d55e49a309313f007a7

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
