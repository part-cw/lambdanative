PKGURL=https://github.com/madler/zlib/releases/download/v1.4/zlib-1.4.tar.gz
PKGHASH=6bb95b58fb638ec49d921557bf66183baa4c6522

package_download $PKGURL $PKGHASH

if [ ! "$SYS_PLATFORM" = "win32" ] || [ `uname | cut -c 1-6` = "CYGWIN" ]; then
  NOQUIET=yupgofigure package_configure --static
else
  PREFIX=`echo "$SYS_STRIP" | sed 's/strip$//;s/ $//'`
  cat win32/Makefile.gcc | sed "s|^PREFIX|PREFIX=$PREFIX#|" > Makefile
fi

package_make 
INCLUDE_PATH="$SYS_PREFIX/include" LIBRARY_PATH="$SYS_PREFIX/lib" BINARY_PATH="$SYS_PREFIX/bin" make install -s
package_cleanup

#eof
