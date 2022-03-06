
PKGURL=http://www.nih.at/libzip/libzip-0.11.2.tar.gz
PKGHASH=eeb3b5567fcf3532fa4bcb6440a87c7ad8507d2d

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

if [ "$SYS_PLATFORM" = "win32" ]; then
  if grep microsoft /proc/version -i -q; then
    EXTRACONF="$EXTRACONF --build=x86_64-unknown-linux-gnu"
  fi
fi

package_configure $EXTRACONF --enable-static --disable-shared

package_patch

cd lib
package_make
package_make install
cd ..
cp $SYS_PREFIX/lib/libzip/include/zipconf.h $SYS_PREFIX/include

package_cleanup

# eof
