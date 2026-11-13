PKGURL=https://ftp.gnu.org/gnu/nettle/nettle-4.0.tar.gz
PKGHASH=8a2f1b1d3c2e4c108b40dbc051a545550c1a6a4c

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared --disable-fat 

package_make

package_make install

package_cleanup

#eof
