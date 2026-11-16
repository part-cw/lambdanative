PKGURL=https://github.com/libusb/libusb/archive/refs/tags/v1.0.30.tar.gz
PKGHASH=3d88a0fc26dd9d084bde8952ce35df746ec13b84

package_download $PKGURL $PKGHASH

veval "./autogen.sh"
asserterror $? "failed to generate configure script"

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared --disable-udev

package_make

package_make install

package_cleanup

#eof
