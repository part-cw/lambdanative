PKGURL=https://www.iro.umontreal.ca/~gambit/download/gambit/v4.7/source/gambc-v4_7_9.tgz
PKGHASH=3af67937b3e3fb11b6fd87040fde7184b2e753f3

package_download $PKGURL $PKGHASH

package_patch

case $SYS_PLATFORM in
ios)
  EXTRACONF=--host=arm
;;
android|bb10|playbook)
  EXTRACONF=--host=arm-eabi
;;
win32*)
  EXTRACONF=--host=i386-mingw32
;;
linux*)
  EXTRACONF=--host=i386-linux
;;
openwrt)
  EXTRACONF=--host=$SYS_OPENWRTTARGET
;;
sitara|carlson-minot)
  EXTRACONF=--host=arm
;;
*)
  EXTRACONF=
;;
esac

if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=
fi
if [ -f $SYS_ROOT/targets/$SYS_PLATFORM/libgambc_enablesinglehost ]; then
  EXTRACONF="$EXTRACONF --enable-single-host"
fi

package_configure $EXTRACONF


if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  package_make
else
  cd lib
  package_make
  cd ..
fi

if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  package_make install
fi
cp lib/libgambc.a $SYS_PREFIX/lib
cp include/gambit*.h $SYS_PREFIX/include

package_cleanup

#eof
