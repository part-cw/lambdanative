
PKGURL=https://prdownloads.sourceforge.net/libpng/libpng-1.6.12.tar.gz
PKGHASH=6bcd6efa7f20ccee51e70453426d7f4aea7cf4bb

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --with-zlib-prefix="$SYS_PREFIX" --enable-static --disable-shared

if [ ! $SYS_HOSTPLATFORM = win32 ]; then
  package_make
else
  # mutilate some default windows makefile - argh!
  cat scripts/makefile.msys | sed "s|CC =|CC=$SYS_CC #|;s|DESTDIR=|DESTDIR=$SYS_PREFIX #|;s|ZLIBDIR=|ZLIBDIR=$SYS_PREFIX/lib #|;s|ZLIBINC=|ZLIBINC=$SYS_PREFIX/include #|" > makefile.msys
  package_make -f ./makefile.msys 
fi

if [ ! $SYS_HOSTPLATFORM = win32 ]; then
  package_make install
else
  cp *.h $SYS_PREFIX/include
  cp libpng.a $SYS_PREFIX/lib
  cp libpng.a $SYS_PREFIX/lib/libpng16.a
  cp libpng-config $SYS_PREFIX/bin
fi

package_cleanup

#eof
