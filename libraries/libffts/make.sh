
PKGURL=http://anthonix.com/ffts/releases/ffts-0.7.tar.gz
PKGHASH=8318dc460413d952d2468c8bc9aa2e484bb72d98

package_download $PKGURL $PKGHASH

package_patch
autoconf

case $SYS_PLATFORM in
ios)
  EXTRAHOST=--host=arm
  EXTRAENABLE=--enable-neon
;;
android)
  EXTRAHOST=--host=arm-eabi
  EXTRAENABLE=--enable-neon
;;
win32*)
  EXTRAHOST=--host=i386-mingw32
  EXTRAENABLE=--enable-sse
;;
linux*)
  EXTRAHOST=--host=i386-linux
  EXTRAENABLE=--enable-sse
;;
macosx)
  EXTRAHOST=
  EXTRAENABLE=--enable-sse
;;
*)
  EXTRAHOST=
  EXTRACONF=
;;
esac

if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=
fi

package_configure $EXTRAHOST $EXTRAENABLE --enable-single

cd src
package_make
package_make install
cd ..

package_cleanup

#eof
