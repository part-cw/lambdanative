PKGURL=https://github.com/PortAudio/portaudio/archive/refs/tags/v19.7.0.tar.gz
PKGHASH=90676f9b7856bf100b396d8f14cc95bbfb91fa40

package_download $PKGURL $PKGHASH

rmifexists $SYS_PREFIX/include/portaudio.h

# openbsd needs this
export AUTOCONF_VERSION=2.69
autoconf

case $SYS_PLATFORM in
ios)
  EXTRACONF=--host=arm
;;
android)
  EXTRACONF=--host=arm-eabi
;;
win32*)
  EXTRACONF=--host=i386-mingw32
;;
linux*)
  EXTRACONF=--host=i386-linux
;;
*)
  EXTRACONF=
;;
esac

if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=
fi

if [ $SYS_HOSTPLATFORM = openbsd ]; then
  EXTRACONF="--without-alsa --without-oss"
fi

if [ "$SYS_PLATFORM" = "win32" ]; then
  if grep microsoft /proc/version -i -q; then
    EXTRACONF="$EXTRACONF --build=x86_64-unknown-linux-gnu"
  fi
fi

package_configure $EXTRACONF --enable-static --disable-shared --without-jack

mv Makefile tmp
cat tmp | sed "s/-arch i386//g;s/-arch x86_64//;s/Werror/Wall/g" > Makefile

package_make
package_make install
if [ $SYS_PLATFORM = macosx ]; then
  cp "include/pa_mac_core.h" "$SYS_PREFIX/include"
fi

package_cleanup

#eof
