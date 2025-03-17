PKGURL=https://download.savannah.gnu.org/releases/freetype/freetype-2.13.3.tar.gz
PKGHASH=f976fa6f5020707e4e8d5a994c7fe27eb53264b3

package_download $PKGURL $PKGHASH
package_patch

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

if [ "$SYS_PLATFORM" = "win32" ]; then
  if grep microsoft /proc/version -i -q; then
    EXTRACONF="$EXTRACONF --build=x86_64-unknown-linux-gnu"
  fi
fi


package_configure $EXTRACONF --enable-static --disable-shared --without-png --without-harfbuzz --without-zlib --without-bzip2 --enable-freetype-config



package_make
package_make install
package_cleanup

#eof
