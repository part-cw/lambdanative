
PKGURL=http://www.portaudio.com/archives/pa_stable_v19_20140130.tgz
PKGHASH=526a7955de59016a06680ac24209ecb6ce05527d

package_download $PKGURL $PKGHASH

package_patch

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

package_configure $EXTRACONF --enable-static --disable-shared --without-jack

mv Makefile tmp
cat tmp | sed "s/-arch i386//g;s/-arch x86_64//;s/Werror/Wall/g" > Makefile

package_make
package_make install

package_cleanup

#eof
