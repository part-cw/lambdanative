#!/bin/sh

PKGURL=http://zlib.net/zlib-1.2.8.tar.gz
PKGHASH=a4d316c404ff54ca545ea71a27af7dbc29817088

package_download $PKGURL $PKGHASH

if [ ! $SYS_PLATFORM = win32 ]; then
  NOQUIET=yupgofigure package_configure --static
else
  PREFIX=`echo "$SYS_STRIP" | sed 's/strip$//;s/ $//'`
  cat win32/Makefile.gcc | sed "s|^PREFIX|PREFIX=$PREFIX#|" > Makefile
fi

package_make
INCLUDE_PATH="$SYS_PREFIX/include" LIBRARY_PATH="$SYS_PREFIX/lib" BINARY_PATH="$SYS_PREFIX/bin" package_make install
package_cleanup

#eof
