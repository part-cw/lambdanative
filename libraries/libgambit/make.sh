PKGURL=https://github.com/gambit/gambit/archive/v4.9.8/gambit-v4.9.8.tar.gz
PKGHASH=a42e699c0872c68a36214e8cc22f504f03ce6daa

package_download $PKGURL $PKGHASH

package_patch

case $SYS_PLATFORM in
ios)
  EXTRACONF=--host=x86_64-apple-darwin
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

package_configure $EXTRACONF --disable-openssl --enable-targets=C


if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  package_make
else
  echo 'exit 0' > gsc-boot && chmod +x gsc-boot
  cd include
  package_make
  cp -p stamp-release.h stamp.h
  cd ..
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
