PKGURL=https://ftp.pcre.org/pub/pcre/pcre-8.43.tar.gz
PKGHASH=8f36ed69d3e938972fc511c19bfaa0ff27ff1d71

package_download $PKGURL $PKGHASH

package_patch

EXTRACONF="--enable-utf --enable-jit "

if [ "$SYS_PLATFORM" != "$SYS_HOSTPLATFORM" ]; then
    EXTRACONF="$EXTRACONF --host=$SYS_ARCH"
fi

# configure

package_configure $EXTRACONF

# build

NOQUIET=yes package_make

# install

cp .libs/libpcre.a .libs/libpcreposix.a $SYS_PREFIX/lib || exit 1
cp pcre.h pcreposix.h pcrecpparg.h pcre_scanner.h pcre_stringpiece.h $SYS_PREFIX/include || exit 1

package_cleanup

unset EXTRACONF

#eof
