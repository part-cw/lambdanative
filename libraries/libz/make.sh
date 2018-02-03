PKGURL=https://github.com/madler/zlib/archive/v1.2.11.zip
PKGHASH=eba1cd6f2e3c7a75fe33a8b02b4d7e4b1ad58481

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
