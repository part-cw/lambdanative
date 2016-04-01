PKGURL=https://github.com/signal11/hidapi.git
PKGHASH=d17db57b9d4354752e0af42f5f33007a42ef2906
package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

asserttool autoconf automake
autoreconf --install

package_configure $EXTRACONF --disable-shared --enable-static
package_make
package_make install

package_cleanup

#eof
