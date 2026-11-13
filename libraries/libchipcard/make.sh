PKGURL=https://www.aquamaniac.de/rdm/attachments/download/382/libchipcard-5.1.6.tar.gz
PKGHASH=76fcb8b2e931fb3a209cd92193613f7c0bf18644

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  EXTRACONF=--host=$SYS_ARCH
fi

package_configure $EXTRACONF --enable-static --disable-shared --enable-gwenhywfar --disable-gwenhywfar-test

package_make

package_make install

package_cleanup

#eof
