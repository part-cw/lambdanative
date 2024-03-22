
PKGURL=https://downloads.sourceforge.net/project/libuuid/libuuid-1.0.3.tar.gz
PKGHASH=46eaedb875ae6e63677b51ec583656199241d597

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
elif [ "$SYS_PLATFORM" = "ios" ]; then
  wget -q -O config.sub 'http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD'
fi

package_configure $EXTRACONF --enable-static --disable-shared
package_make
package_make install

# move the headers to prevent potential clash with system headers
if [ -d $SYS_PREFIX/include/uuid ]; then
  mv $SYS_PREFIX/include/uuid $SYS_PREFIX/include/ln_uuid
fi

package_cleanup

#eof
