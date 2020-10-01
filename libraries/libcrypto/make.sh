PKGURL=https://www.openssl.org/source/openssl-1.1.1h.tar.gz
PKGHASH=8d0d099e8973ec851368c8c775e05e1eadca1794

package_download $PKGURL $PKGHASH

package_patch
rmifexists $SYS_PREFIX/include/openssl

EXTRACONF=
case $SYS_PLATFORM in
win32*)
  EXTRACONF="no-engine mingw"
;;
linux486*)
  EXTRACONF=linux-generic32
;;
linux*)
  cpu=
  if [ $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
    cpu=`$SYS_ROOT/config.guess | cut -f 1 -d "-"`
  fi
  if [ "X$cpu" = "Xx86_64" ]; then
    EXTRACONF=linux-x86_64
  else
    EXTRACONF=linux-generic32
  fi
;;
macosx)
  if [ "$KERNEL_BITS" = "32" ]; then
      EXTRACONF="darwin-i386-cc"
  else
      EXTRACONF="darwin64-x86_64-cc"
  fi
;;
android*)
  EXTRACONF="android-$SYS_CPU -D__ANDROID_API__=$SYS_ANDROIDAPI"
  export ANDROID_NDK_HOME=$android_customtoolchain
  PATH=$android_customtoolchain/bin:$PATH
;;
ios*)
  SDK=`xcrun --sdk iphoneos --show-sdk-path`
  if [ "$SYS_CPU" = "arm64" ]; then
    EXTRACONF=ios64-xcrun
  else
    EXTRACONF=ios-xcrun
  fi
;;
*)
  EXTRACONF=BSD-generic32
;;
esac

case $SYS_PLATFORM in
    # Openssl 1.1.x appears take great care to defeat the use of
    # `package_configure` from lambdanative when cross compiling for
    # Android.
    android)
	./Configure --openssldir="$SYS_PREFIX" no-shared no-threads no-zlib no-asm no-dso no-sse2 $EXTRACONF # CC=$SYS_CC
    ;;
    *)
	cp Configure configure   # openssl ships a Configure script
	NOQUIET=yes package_configure --openssldir="$SYS_PREFIX" no-shared no-threads no-zlib no-asm no-dso no-sse2 $EXTRACONF # CC=$SYS_CC
;;
esac
package_make build_libs

cp *.a $SYS_PREFIX/lib
cp -L -R include/openssl $SYS_PREFIX/include

package_cleanup

#eof
