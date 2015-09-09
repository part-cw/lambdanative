
PKGURL=https://github.com/madler/zlib.git
PKGHASH=50893291621658f355bc5b4d450a8d06a563053d

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
