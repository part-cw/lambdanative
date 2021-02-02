<<<<<<< Updated upstream
PKGURL=https://www.lambdanative.org/ffts-0.7/ffts-0.7.tar.gz
PKGHASH=8318dc460413d952d2468c8bc9aa2e484bb72d98
#PKGURL=https://github.com/linkotec/ffts.git
#PKGHASH=8e3dee08b1d156a779050c288e9016fadbb0f369
=======
PKGURL=https://github.com/linkotec/ffts.git
PKGHASH=dfab21f8096660f441fb33bf5012e7f2c3652fa9
>>>>>>> Stashed changes

package_download $PKGURL $PKGHASH

package_patch
autoconf

case $SYS_PLATFORM in
ios)
  EXTRAHOST=--host=arm-eabi
  EXTRAENABLE=--enable-neon
;;
android)
  EXTRAHOST=--host=arm-eabi
  EXTRAENABLE=--enable-neon
;;
win32*)
  EXTRAHOST=--host=i386-mingw32
  EXTRAENABLE=
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
cp ../include/ffts.h .
package_make
package_make install
cd ..

package_cleanup

#eof
