PKGURL=https://downloads.xiph.org/releases/opus/opus-1.6.1.tar.gz
PKGHASH=74612396b53f0c0240d2f81d4c7b0eaa14620402

package_download $PKGURL $PKGHASH

package_patch

case $SYS_PLATFORM in
ios)
  EXTRACONF=--host=arm
;;
android)
  EXTRACONF=--host=arm-eabi
;;
win32*)
  EXTRACONF="--host=$SYS_ARCH"
;;
linux*)
  EXTRACONF="--host=$SYS_ARCH"
;;
*)
  EXTRACONF=
;;
esac

if [ "$SYS_PLATFORM" = "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF=
fi

export CFLAGS="$CFLAGS -DHAVE_CONFIG_H -DOPUS_BUILD -I."
case $SYS_PLATFORM in
win32*)
  export CFLAGS="$CFLAGS -DVAR_ARRAYS"
  EXTRACONF="$EXTRACONF --disable-intrinsics"
;;
esac

package_configure $EXTRACONF --enable-static --disable-shared

package_make
package_make install
package_cleanup

#eof
