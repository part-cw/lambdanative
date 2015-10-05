
PKGURL=ftp://xmlsoft.org/libxml2/libxml2-2.9.1.tar.gz
PKGHASH=eb3e2146c6d68aea5c2a4422ed76fe196f933c21

package_download $PKGURL $PKGHASH

EXTRACONF=
if [ ! $SYS_PLATFORM = $SYS_HOSTPLATFORM ]; then
  if [ $SYS_PLATFORM = ios ]; then
    EXTRACONF=--host=arm-apple-darwin
  else
    EXTRACONF=--host=$SYS_ARCH
  fi
fi

package_configure $EXTRACONF --enable-static --disable-shared --without-python --without-readline --without-iconv

package_patch

package_make libxml2.la
assertfile .libs/libxml2.a

rm -rf $SYS_PREFIX/include/libxml 2> /dev/null
cp -R include/libxml $SYS_PREFIX/include
cp .libs/libxml2.a $SYS_PREFIX/lib

package_cleanup

# eof
