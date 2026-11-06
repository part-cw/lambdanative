PKGURL=http://prdownloads.sourceforge.net/libpng/libpng-1.6.59.tar.gz
PKGHASH=debbdb4fafd99a3dffdd9b3895cd097ca0fe3ed4

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi
#Previous headers break the build process, so remove them
rmifexists $SYS_PREFIX/include/png*

package_configure $EXTRACONF --with-zlib-prefix="$SYS_PREFIX" --enable-static --disable-shared

if [ ! $SYS_HOSTPLATFORM = win32 ]; then
  if [ $SYS_PLATFORM = android ] && [ -f $SYS_PREFIX/include/zlib.h ] ; then
    mv $SYS_PREFIX/include/zlib.h $SYS_PREFIX/include/zlib.h.tmp
    package_make
    mv $SYS_PREFIX/include/zlib.h.tmp $SYS_PREFIX/include/zlib.h
  else
    package_make
  fi
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
