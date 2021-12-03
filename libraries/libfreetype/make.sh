PKGURL=https://download.savannah.gnu.org/releases/freetype/freetype-2.10.1.tar.gz
PKGHASH=3296b64ad1e7540289f22e4b6383e26e928b0a20

package_download $PKGURL $PKGHASH
package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared --without-png --without-harfbuzz --without-zlib --without-bzip2 --enable-freetype-config

package_make
package_make install
package_cleanup

#eof
