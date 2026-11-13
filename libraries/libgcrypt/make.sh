PKGURL=https://www.gnupg.org/ftp/gcrypt/libgcrypt/libgcrypt-1.12.2.tar.bz2
PKGHASH=7b8ff21966a0b6e7a735466b9b9b55d9dac9fa87

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared --disable-doc

package_make

package_make install

package_cleanup

#eof
