PKGURL=https://www.openssl.org/source/openssl-1.0.2.tar.gz
PKGHASH=2f264f7f6bb973af444cd9fc6ee65c8588f610cc

package_download $PKGURL $PKGHASH

package_patch
rmifexists $SYS_PREFIX/include/openssl

EXTRACONF=
case $SYS_PLATFORM in
win32*)
  EXTRACONF=mingw
;;
linux486*)
  EXTRACONF=linux-generic32
;;
linux*)
  cpu=`$SYS_ROOT/config.guess | cut -f 1 -d "-"`
  if [ "X$cpu" = "Xx86_64" ]; then
    EXTRACONF=linux-x86_64
  else
    EXTRACONF=linux-generic32
  fi
;;
*)
  EXTRACONF=BSD-generic32
;;
esac

cp Configure configure   # openssl ships a Configure script
NOQUIET=yes package_configure --openssldir="$SYS_PREFIX" $EXTRACONF no-shared no-threads no-zlib no-asm no-dso no-sse2
package_make build_ssl build_crypto

cp *.a $SYS_PREFIX/lib
cp -L -R include/openssl $SYS_PREFIX/include

package_cleanup

#eof
