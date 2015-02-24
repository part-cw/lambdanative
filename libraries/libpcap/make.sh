
if [ "$SYS_PLATFORM" = "win32" ]; then
  PKGURL=http://www.winpcap.org/install/bin/WpdPack_4_1_2.zip
  PKGHASH=f5c80885bd48f07f41833d0f65bf85da1ef1727a
  package_download $PKGURL $PKGHASH
  echo " => Installing precompiled library..."
  cp Lib/libwpcap.a $SYS_PREFIX/lib/libpcap.a
  cp Lib/libpacket.a $SYS_PREFIX/lib/
  cp -L -R Include/ $SYS_PREFIX/include/
  package_cleanup  
  cd $here
  continue # continues the libary building loop here
else
  PKGURL=http://www.tcpdump.org/release/libpcap-1.4.0.tar.gz
  PKGHASH=9c9710aab68be58ed1d41b5c36dc2599419a80e0
fi

package_download $PKGURL $PKGHASH

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
  EXTRACONF="--host=i386-linux --with-pcap=linux"
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

EXTRACONF="$EXTRACONF --disable-ipv6 --disable-bluetooth --disable-canusb --disable-can --without-libnl --disable-shared"
package_configure $EXTRACONF

package_make
INCLUDE_PATH="$SYS_PREFIX/include" LIBRARY_PATH="$SYS_PREFIX/lib" BINARY_PATH="$SYS_PREFIX/bin" package_make install
package_cleanup

