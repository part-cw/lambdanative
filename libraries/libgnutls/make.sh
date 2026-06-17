PKGURL=https://www.gnupg.org/ftp/gcrypt/gnutls/v3.7/gnutls-3.7.11.tar.xz
PKGHASH=9001ec4c4ae9decd9aeed13fc6a9ca60b4ffb3a7

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared --disable-tests --disable-doc --disable-doc --disable-tools  

package_make

package_make install

package_cleanup

#eof
