PKGURL=https://github.com/LudovicRousseau/PCSC/archive/refs/tags/2.4.1.tar.gz
PKGHASH=4f8215f6ddf1015dd1d37c851b4c4119b627dae6

package_download $PKGURL $PKGHASH

veval "./bootstrap"
asserterror $? "failed to generate configure script"

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared  --disable-libudev

package_make

package_make install

package_cleanup

#eof
