
PKGURL=http://downloads.xiph.org/releases/opus/opus-1.1.tar.gz
PKGHASH=35005f5549e2583f5770590135984dcfce6f3d58

package_download $PKGURL $PKGHASH

package_patch
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

if [ "$SYS_PLATFORM" = "win32" ]; then
  if grep microsoft /proc/version -i -q; then
    EXTRACONF="$EXTRACONF --build=x86_64-unknown-linux-gnu"
  fi
fi

package_configure $EXTRACONF  --enable-static --disable-shared

mv Makefile tmp
cat tmp | sed "s/-arch i386//g;s/-arch x86_64/-m32/" > Makefile

package_make
package_make install
package_cleanup

#eof
