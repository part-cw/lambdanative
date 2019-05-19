# DON'T TRY 4.9.3 gambit issue #384 define-syntax is broken
# see https://github.com/gambit/gambit/issues/384
# PKGURL=https://www.iro.umontreal.ca/~gambit/download/gambit/v4.9/source/gambit-v4_9_3.tgz
# PKGHASH=a3f836f732f367601d0df8bedb8e1324a14c413b
PKGURL=https://www.iro.umontreal.ca/~gambit/download/gambit/v4.9/source/gambit-v4_9_2.tgz
PKGHASH=70c2e66cdbaf544afdcfa6d396c37f72ec67e934
PKGURL=https://github.com/gambit/gambit/archive/v4.8.5.tar.gz
PKGHASH=08d129513d04970f30302f28cac17c4e21a25a6b

package_download $PKGURL $PKGHASH

package_patch

case $SYS_PLATFORM in
ios)
  EXTRACONF=--host=arm
  cp configure configure.tmp
  cat configure.tmp | sed 's/#define HAVE_CLOCK_GETTIME/\/\/#define HAVE_CLOCK_GETTIME/g' > configure
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
cp lib/libgambit.a $SYS_PREFIX/lib
cp include/gambit*.h $SYS_PREFIX/include

package_cleanup

#eof
